/// The compiler lazily interprets the file
const std = @import("std");
const context = @import("context.zig");
const State = @import("state.zig");
const Ast = @import("ast.zig");
const AstIn = Ast.Ast;
const Parse = @import("parse.zig");

const Self = @This();
// const var_map = std.AutoArrayHashMapUnmanaged([]const u8, u32);
const var_info = struct {
    ptr_addr: u32,
    type: Ast.Expr,
    is_const: bool,
};
// const var_map = std.StringArrayHashMapUnmanaged(u32); // Map to pointer address of vars in scope
const VarMap = std.StringArrayHashMapUnmanaged(var_info); // Map to pointer address of vars in scope
const HeapStr = std.ArrayListUnmanaged(u8);

const StackFrame = struct {
    vars: VarMap,
    var_ptr: u32 = 0,
};

const var_stack = std.ArrayListUnmanaged(StackFrame);

// state: State,
ctx: *context,
alloc: std.mem.Allocator,
asm_str: std.ArrayListUnmanaged(u8),
vars: var_stack,
static_vars: VarMap,
buf: [100]u8,

data_queue_gen: HeapStr, // generate section .data
fn_queue_gen: HeapStr, // generate function assembly block

// main: ?*Ast., // TODO: TBD

pub fn eval_type_expr(_: *Self, expr: Ast.Expr) !Ast.Expr {
    return switch (expr) {
        .val_bool, .val_char, .val_lit, .val_num => expr,
    };
}

fn gen_data_section(self: *Self) !void {
    // var str = HeapStr{};
    const len = self.data_queue_gen.items.len;

    try self.asm_str.insertSlice(self.alloc, 0, "\n");
    try self.asm_str.insertSlice(self.alloc, 0, self.data_queue_gen.items[0..len]);
    try self.asm_str.insertSlice(self.alloc, 0, "section .data\n");
}

/// All the assembly is in self.ast_str
fn gen_asm(self: *Self, ast: AstIn) !void {
    try self.asm_str.appendSlice(self.alloc, section ++ " " ++ dtext ++ "\n");
    try self.asm_str.appendSlice(self.alloc, global ++ " " ++ "main\n");
    try self.asm_str.appendSlice(self.alloc, "main:\n");

    try self.asm_str.appendSlice(self.alloc, "push rbp\n"); // TODO:This will be removed when main() is mandatory starting fn
    try self.asm_str.appendSlice(self.alloc, "mov rbp, rsp\n");

    switch (ast) {
        .stmts => |s| try self.gen_asm_stmts(s),
        else => unreachable,
    }

    try self.asm_str.appendSlice(self.alloc, "xor eax, eax\n");
    try self.asm_str.appendSlice(self.alloc, "pop rbp\n");
    try self.asm_str.appendSlice(self.alloc, "ret\n");

    try self.gen_data_section();
}

fn gen_asm_stmts(self: *Self, stmts: Ast.Statements) !void {
    try gen_asm_stmt(self, stmts.statement);

    if (stmts.statements) |s|
        try gen_asm_stmts(self, s.*);
}

fn gen_asm_stmt(self: *Self, stmt: Ast.Statement) !void {
    const str = switch (stmt) {
        .assign => |a| try self.gen_asm_assign_or_reassign(Ast.AssignReassign{ .assign = a }),
        .reassign => |r| try self.gen_asm_assign_or_reassign(Ast.AssignReassign{ .reassign = r }),

        // else => return error.Undefined,
    };
    try self.asm_str.appendSlice(self.alloc, str);
}

fn gen_asm_assign_or_reassign(self: *Self, ar: Ast.AssignReassign) ![]const u8 {
    // const is_const = assign.@"var".is_const;
    // const curr_stack = &self.vars.getLast();
    const Res = struct { size: u32, str_len: usize };
    const curr_stack = &self.vars.items[self.vars.items.len - 1];

    const scope = "global.fn";

    const assign = switch (ar) {
        .assign => |a| a.assign,
        .reassign => |r| r.assign,
    };

    const ident = switch (ar) {
        .assign => |a| a.@"var".ident,
        .reassign => |r| r.ident,
    };

    std.log.debug("gen_asm_assign ptr before: {}\n", .{curr_stack.var_ptr});

    self.buf = std.mem.zeroes([self.buf.len]u8);

    const res = asm_lbl: {
        _ = switch (assign) {
            .expr => |e| {
                _ = switch (e) {
                    .val_lit => {
                        const size = 8;

                        const loc = if (self.static_vars.get(e.val_lit)) |g| g.ptr_addr else s: {
                            const len = self.static_vars.entries.len;

                            const slc = try std.fmt.bufPrint(&self.buf, "{s}.{s} db \'{s}\', 0", .{ scope, ident, e.val_lit });
                            try self.data_queue_gen.appendSlice(self.alloc, slc);

                            break :s @as(u32, @intCast(len));
                        };
                        _ = loc;

                        const ptr_type_str = qword_ptr;
                        const ptr = switch (ar) {
                            .assign => curr_stack.*.var_ptr + size,
                            .reassign => |r| if (self.vars.getLast().vars.get(r.ident)) |g| g.ptr_addr else return error.VarNotFoundInScope,
                        };

                        const s = try std.fmt.bufPrint(&self.buf, "mov {s} [rbp-{}], {s}.{s}\n", .{
                            ptr_type_str,
                            ptr,
                            scope,
                            ident,
                        });

                        break :asm_lbl Res{ .size = size, .str_len = s.len };
                    },
                    .val_num => |n| {
                        const size = @bitSizeOf(f64);

                        const ptr = switch (ar) {
                            .assign => curr_stack.*.var_ptr + size,
                            .reassign => |r| if (self.vars.getLast().vars.get(r.ident)) |g| g.ptr_addr else return error.VarNotFoundInScope,
                        };

                        // const ptr_type_str = qword_ptr;
                        const param: u64 = @bitCast(n);

                        const s = try std.fmt.bufPrint(&self.buf,
                            \\mov rax, {d}
                            \\mov [rbp-{}], rax
                            \\
                        , .{
                            param, ptr,
                        });

                        break :asm_lbl Res{ .size = size, .str_len = s.len };
                    },

                    inline .val_bool, .val_char => |n| {
                        const size = 8;

                        const ptr = switch (ar) {
                            .assign => curr_stack.*.var_ptr + size,
                            .reassign => |r| if (self.vars.getLast().vars.get(r.ident)) |g| g.ptr_addr else return error.VarNotFoundInScope,
                        };

                        const ptr_type_str = byte_ptr;

                        const param: u64 = switch (@TypeOf(n)) {
                            bool => if (n == true) 1 else 0,
                            u8 => n,
                            else => return error.Undefined,
                        };

                        const s = try std.fmt.bufPrint(&self.buf, "mov {s} [rbp-{}], {d}\n", .{ ptr_type_str, ptr, param });

                        break :asm_lbl Res{ .size = size, .str_len = s.len };
                    },
                };
            },
        };

        return error.undefined;
    };

    // try self.asm_str.appendSlice(self.alloc, asm_str);

    // std.log.debug("gen_asm_assign ptr mid: {}\n", .{curr_stack.var_ptr});
    switch (ar) {
        .assign => |a| {
            curr_stack.var_ptr += res.size;
            const info = var_info{ .is_const = a.@"var".is_const, .ptr_addr = curr_stack.var_ptr, .type = a.assign.expr };
            // try curr_stack.vars.put(self.alloc, ident, curr_stack.var_ptr);
            try curr_stack.vars.put(self.alloc, ident, info);
        },
        else => {},
    }
    std.log.debug("gen_asm_assign ptr after: {}\n", .{curr_stack.var_ptr});

    std.log.info("gen_asm_assign: {s}\n", .{self.buf[0..res.str_len]});

    return self.buf[0..res.str_len];
}

pub fn new_compiler(alloc: std.mem.Allocator, ctx: *context) !Self {
    std.log.warn("There are memory leaks, need to use arena allocator\n", .{});
    var arr = try std.ArrayListUnmanaged(StackFrame).initCapacity(alloc, 64);
    const stk = StackFrame{ .var_ptr = 0, .vars = VarMap{} };
    try arr.append(alloc, stk);

    return .{
        .alloc = alloc,
        .ctx = ctx,
        // .vars = try std.ArrayListUnmanaged(StackFrame).initCapacity(alloc, 64),
        .vars = arr,
        .asm_str = try std.ArrayListUnmanaged(u8).initCapacity(alloc, 64),
        .static_vars = VarMap{},
        .data_queue_gen = HeapStr{},
        .fn_queue_gen = HeapStr{},
        // .buf = std.mem.zeroes([50]u8),
        .buf = undefined,
    };
}

pub fn compile(self: *Self) !void {
    const ast = try Parse.parse_stmts(self.ctx, self.alloc);
    try self.gen_asm(ast);
}

pub fn deinit(self: *Self) void {
    self.asm_str.deinit(self.alloc);
    self.vars.deinit(self.alloc);
}

// fn gen_asm_expr(expr: Ast.Expr) !void {
//     switch (expr) {}
// }

// pub const f = fn () void{};

const bool_str = "const x = false;";
const bool_bool_str = "const x = false; const y = false;";
const num_str = "const x = 3;";
const lit_str = "const x = \"abcd\";";
const multi_assign = "const x = 45; var y = true; var z = 1.234;";
const reassign = "const x = 45; x = 50;";
const multi_assign_reassign = "const x = 45; var y = true; var z = 1.234; y = false; const a = \"abcd\";";

test "compiler bool" {
    var memory = std.heap.DebugAllocator(.{}).init;
    // defer _ = memory.deinit();
    const alloc = memory.allocator();

    var ctx = context.new_context(bool_str);

    var cmp = try new_compiler(alloc, &ctx);
    defer cmp.deinit();

    _ = try cmp.compile();
    std.log.info("{s}", .{cmp.asm_str.items});

    std.log.info("finished compiler test bool", .{});
}

test "compiler bool_bool" {
    var memory = std.heap.DebugAllocator(.{}).init;
    // defer _ = memory.deinit();
    const alloc = memory.allocator();

    var ctx = context.new_context(bool_bool_str);

    var cmp = try new_compiler(alloc, &ctx);
    defer cmp.deinit();

    _ = try cmp.compile();
    std.log.info("{s}", .{cmp.asm_str.items});

    std.log.info("finished compiler test 2", .{});
}

test "compiler num" {
    var memory = std.heap.DebugAllocator(.{}).init;
    // defer _ = memory.deinit();
    const alloc = memory.allocator();
    var ctx = context.new_context(num_str);
    var cmp = try new_compiler(alloc, &ctx);
    defer cmp.deinit();

    _ = try cmp.compile();
    std.log.info("{s}", .{cmp.asm_str.items});

    std.log.info("finished compiler test num", .{});
}

test "compiler lit" {
    var memory = std.heap.DebugAllocator(.{}).init;
    // defer _ = memory.deinit();
    const alloc = memory.allocator();
    // const alloc = std.testing.allocator;
    var ctx = context.new_context(lit_str);
    var cmp = try new_compiler(alloc, &ctx);
    defer cmp.deinit();

    try cmp.compile();
    std.log.info("{s}", .{cmp.asm_str.items});

    std.log.info("{s}", .{cmp.asm_str.items});
    std.log.info("finished compiler test lit", .{});
}

test "compiler multi_assign" {
    var memory = std.heap.DebugAllocator(.{}).init;
    // defer _ = memory.deinit();
    const alloc = memory.allocator();
    // const alloc = std.testing.allocator;
    var ctx = context.new_context(multi_assign);
    var cmp = try new_compiler(alloc, &ctx);
    defer cmp.deinit();

    try cmp.compile();

    std.log.info("{s}", .{cmp.asm_str.items});
    std.log.info("finished compiler test multi_assign", .{});
}

test "compiler reassign" {
    var memory = std.heap.DebugAllocator(.{}).init;
    // defer _ = memory.deinit();
    const alloc = memory.allocator();
    // const alloc = std.testing.allocator;
    var ctx = context.new_context(reassign);
    var cmp = try new_compiler(alloc, &ctx);
    defer cmp.deinit();

    _ = try cmp.compile();
    std.log.info("finished compiler test reassign", .{});
}

test "compiler multi_assign_reassign" {
    var memory = std.heap.DebugAllocator(.{}).init;
    // defer _ = memory.deinit();
    const alloc = memory.allocator();
    // const alloc = std.testing.allocator;
    var ctx = context.new_context(multi_assign_reassign);
    var cmp = try new_compiler(alloc, &ctx);
    defer cmp.deinit();

    _ = try cmp.compile();
    std.log.info("{s}", .{cmp.asm_str.items});

    const fpath = "tmp";

    std.log.info("PATH: {s}\n\n\n\n", .{fpath});

    const f = try std.fs.cwd().createFile(fpath ++ ".asm", .{});
    // defer std.fs.cwd().deleteFile(fpath) catch {};

    try f.writeAll(cmp.asm_str.items);
    var child = std.process.Child.init(&.{ "nasm", "-f", "elf64", "-o", fpath ++ ".o", fpath ++ ".asm" }, alloc);
    var term = try child.spawnAndWait();
    switch (term) {
        .Exited => |e| if (e != 0) std.log.err("Err", .{}),
        .Signal => std.log.err("Err", .{}),
        else => {},
    }

    child = std.process.Child.init(&.{ "gcc", "-no-pie", "-s", "-o", fpath, fpath ++ ".o" }, alloc);
    term = try child.spawnAndWait();
    switch (term) {
        .Exited => |e| if (e != 0) std.log.err("Err", .{}),
        .Signal => std.log.err("Err", .{}),
        else => {},
    }

    child = std.process.Child.init(&.{"./" ++ fpath}, alloc);
    term = try child.spawnAndWait();
    switch (term) {
        .Exited => |e| if (e != 0) std.log.err("Err", .{}),
        .Signal => std.log.err("Err", .{}),
        else => {},
    }

    // try std.process.Child.init(&.{ "gcc -s -o", abs_path, abs_path, ".o" }, alloc).wait();

    std.log.info("finished compiler test multi_assign_reassign", .{});
}

const section = "section";
const dtext = ".text";
const ddata = ".data";
const global = "global";
const qword_ptr = "qword"; // "qword ptr" in as
const dword_ptr = "dword"; // "dword ptr" in as
const byte_ptr = "byte"; // "dword ptr" in as

// const AsmMap = std.StaticStringMap(Token).initComptime();

// const Asm = if (true) struct {
//     eax: []const u8 = "eax",
//     rax: []const u8 = "rax",
//     jmp: []const u8 = "jmp",
//     mov: []const u8 = "mov",
//     cmp: []const u8 = "cmp",
//     // rex_pref : []const u8= "",
//     dword_ptr: []const u8 = "dword ptr",
//     qword_ptr: []const u8 = "qword ptr",
//     rbp: []const u8 = "rbp",
//     // push = "push",
//     // pop = "pop",
//     // ret = "ret",
//     // rsp = "rsp",
// }{} else @compileError("Need to implement assembly instruction");

// TODO: To assembly later
// const Asm = enum(i32) {
//     rax_mov = 0x48_b8, // 48 - REX prefix; b8 OPCODE for mov rax, imm64 -> immediate 64bit value
//     eax_mov = 0xb8,
//     rex_prefix = 0x48,
// };

// fn gen_asm_assign(_: *Self, assign: Ast.Assign) void {
//     _ = assign;
// }

// fn gen_asm_add(_: *Self, add: Ast.BinOperation) !void {
//     const l = add.lhs;
//     const r = add.rhs;

//     const E = struct {
//         pub fn eval_expr_local(e: Ast.Expr) void {
//             return switch (e) {
//                 .val_bool => {},
//                 else => undefined,
//             };
//         }
//     };

//     _ = E.eval_expr_local;

//     asm volatile ("syscall"
//         : [a] "ldr" (l),
//         : [b] "ldr" (r),
//     );

//     // asm (call malloc)
// }

// fn gen_asm(self: *Self, ast: AstIn) !void {
//     switch (ast) {
//         .stmts => |s| {
//             _ = try gen_asm(self, AstIn{ .stmt = s.statement });
//             if (s.statements) |stmts|
//                 _ = try gen_asm(self, AstIn{ .stmts = stmts.* });
//         },
//         .stmt => |s| {
//             const str = switch (s) {
//                 .assign => |a| try self.gen_asm_assign_or_reassign(Ast.AssignReassign{ .assign = a }),
//                 .reassign => |r| try self.gen_asm_assign_or_reassign(Ast.AssignReassign{ .reassign = r }),

//                 // else => return error.Undefined,
//             };
//             try self.asm_str.appendSlice(self.alloc, str);
//         },
//         else => return error.Unexpected,
//     }

//     // return error.Todo;
// }
