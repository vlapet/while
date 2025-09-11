/// The compiler lazily interprets the file
const std = @import("std");
const context = @import("context.zig");
const State = @import("state.zig");
const Ast = @import("ast.zig");
const AstIn = Ast.Ast;
const Parse = @import("parse.zig");

const Self = @This();
// const var_map = std.AutoArrayHashMapUnmanaged([]const u8, u32);
const VarInfo = struct {
    ptr_addr: u32,
    type: Ast.VarType,
    is_const: bool,
};
// const var_map = std.StringArrayHashMapUnmanaged(u32); // Map to pointer address of vars in scope
const VarMap = std.StringArrayHashMapUnmanaged(VarInfo); // Map to pointer address of vars in scope
const HeapStr = std.ArrayListUnmanaged(u8);

const StackFrame = struct {
    vars: VarMap = VarMap{},
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

pub fn eval_type_expr(self: *Self, expr: Ast.Expr) !Ast.VarType {
    return switch (expr) {
        .basic_var => |b| switch (b) {
            inline .val_bool,
            .val_char,
            .val_lit,
            .val_num,
            .val_void,
            => expr.basic_var,
            // else => @panic("message: []const u8"),
        },
        .block => |b| {
            // _ = b;
            if (b.expr) |e| {
                return try self.eval_type_expr(e.*);
            } else {
                return .val_void;
            }
        },
        .@"if" => std.debug.panic("unimplemented\n", .{}),
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
        .block => return error.Todo,
        .@"if" => return error.Todo,

        // else => return error.Undefined,
    };
    try self.asm_str.appendSlice(self.alloc, str);
}

fn gen_asm_assign_or_reassign(self: *Self, ar: Ast.AssignReassign) anyerror![]const u8 {
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
            .basic_var => |b| {
                var ret_slc: []u8 = undefined;
                var size: u32 = undefined;
                switch (b) {
                    .val_lit => |v| {
                        size = 8;

                        const loc = if (self.static_vars.get(v)) |g| g.ptr_addr else s: {
                            const len = self.static_vars.entries.len;

                            const slc = try std.fmt.bufPrint(&self.buf, "{s}.{s} db \'{s}\', 0\n", .{ scope, ident, v });
                            try self.data_queue_gen.appendSlice(self.alloc, slc);

                            break :s @as(u32, @intCast(len));
                        };
                        _ = loc;

                        const ptr_type_str = qword_ptr;
                        const ptr = switch (ar) {
                            .assign => curr_stack.*.var_ptr + size,
                            .reassign => |r| if (self.vars.getLast().vars.get(r.ident)) |g| g.ptr_addr else return error.VarNotFoundInScope,
                        };

                        ret_slc = try std.fmt.bufPrint(&self.buf, "mov {s} [rbp-{}], {s}.{s}\n", .{
                            ptr_type_str,
                            ptr,
                            scope,
                            ident,
                        });

                        // break :asm_lbl Res{ .size = size, .str_len = s.len };
                    },
                    .val_num => |n| {
                        size = @bitSizeOf(f64);

                        const ptr = switch (ar) {
                            .assign => curr_stack.*.var_ptr + size,
                            .reassign => |r| if (self.vars.getLast().vars.get(r.ident)) |g| g.ptr_addr else return error.VarNotFoundInScope,
                        };

                        // const ptr_type_str = qword_ptr;
                        const param: u64 = @bitCast(n);

                        ret_slc = try std.fmt.bufPrint(&self.buf,
                            \\mov rax, {d}
                            \\mov [rbp-{}], rax
                            \\
                        , .{
                            param, ptr,
                        });

                        // break :asm_lbl Res{ .size = size, .str_len = s.len };
                    },

                    inline .val_bool, .val_char => |n| {
                        size = 8;

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

                        ret_slc = try std.fmt.bufPrint(&self.buf, "mov {s} [rbp-{}], {d}\n", .{ ptr_type_str, ptr, param });

                        // break :asm_lbl Res{ .size = size, .str_len = s.len };
                    },
                    else => std.debug.panic("Unimplemented\n", .{}),
                }
                break :asm_lbl Res{ .size = size, .str_len = ret_slc.len };
            },
            .block => |b| {
                // assign nothing
                std.log.warn("EMPTY BLOCK SHOULD BE TYPE VOID!\n", .{});
                if (b.stmts == null and b.expr == null) {
                    break :asm_lbl Res{ .size = 0, .str_len = 0 };
                }

                var str = HeapStr{};

                if (b.stmts) |stmts| {
                    std.log.info("generating asm for block stmts\n", .{});
                    try self.vars.append(self.alloc, StackFrame{ .var_ptr = curr_stack.var_ptr }); // Inner scope visible to compiler only

                    try self.gen_asm_stmts(stmts.*);

                    const sf: StackFrame = self.vars.pop().?;
                    curr_stack.*.var_ptr = sf.var_ptr;
                }

                _ = if (b.expr) |expr| {
                    std.log.info("generating asm for block stmts\n", .{});
                    switch (expr.*) {
                        .basic_var => |v| {
                            const a = switch (ar) {
                                .assign => |as| Ast.AssignReassign{
                                    // .assign = .{ .@"var" = as.@"var", .assign = .{
                                    //     .expr = .{ .basic_var = v },
                                    // } },
                                    .assign = .{ .@"var" = as.@"var", .assign = .{ .basic_var = v } },
                                },
                                .reassign => |rs| Ast.AssignReassign{
                                    // .reassign = .{ .ident = rs.ident, .assign = .{
                                    //     .expr = .{ .basic_var = v },
                                    // } },
                                    .reassign = .{ .ident = rs.ident, .assign = .{ .basic_var = v } },
                                },
                            };
                            const slc = try self.gen_asm_assign_or_reassign(a);
                            std.debug.print("WARNING: FINISH BLOCK\n", .{});

                            // return slc;
                            try str.appendSlice(self.alloc, slc);
                        },
                        .block => std.debug.panic("unimplemented\n", .{}),
                        .@"if" => std.debug.panic("unimplemented\n", .{}),
                    }
                };

                if (b.stmts) |_| {
                    // try str.appendSlice(self.alloc, "ret\n");
                }

                return str.items;

                // return error.Todo;
            },
            .@"if" => std.debug.panic("unimplemented", .{}),
        };

        return error.undefined;
    };

    // try self.asm_str.appendSlice(self.alloc, asm_str);

    // std.log.debug("gen_asm_assign ptr mid: {}\n", .{curr_stack.var_ptr});
    switch (ar) {
        .assign => |a| {
            curr_stack.var_ptr += res.size;
            // const a_type = try self.eval_type_expr(a.assign.expr);
            const a_type = try self.eval_type_expr(a.assign);

            // const info = var_info{ .is_const = a.@"var".is_const, .ptr_addr = curr_stack.var_ptr, .type = a.assign.expr };
            const info = VarInfo{
                .is_const = a.@"var".is_const,
                .ptr_addr = curr_stack.var_ptr,
                .type = a_type,
            };
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
const str_str = "const x = \"abcd\";const y = \"abcd\";";
const multi_assign = "const x = 45; var y = true; var z = 1.234;";
const reassign = "const x = 45; x = 50;";
const multi_assign_reassign = "const x = 45; var y = true; var z = 1.234; y = false; const a = \"abcd\";";

const assign_block_empty = "const x = { };";
const assign_block = "const x = { 3 };";
const assign_block2 = "const x = { const y = 4; 3 };";
const assign_nested = "const x = { { 3 } };";
const assign_nested_seq = "const x = { {}; { 3 } };";

test "compiler bool" {
    try compiler_test_generic_str(
        bool_str,
        "bool",
    );
}

test "compiler bool_bool" {
    try compiler_test_generic_str(
        bool_bool_str,
        "bool_bool",
    );
}

test "compiler num" {
    try compiler_test_generic_str(
        num_str,
        "num",
    );
}

test "compiler lit" {
    try compiler_test_generic_str(
        lit_str,
        "lit",
    );
}

test "compiler str_str" {
    try compiler_test_generic_str(
        str_str,
        "str_str",
    );
}

test "compiler multi_assign" {
    try compiler_test_generic_str(
        multi_assign,
        "multi_assign",
    );
}

test "compiler reassign" {
    try compiler_test_generic_str(
        reassign,
        "reassign",
    );
}

test "compiler multi_assign_reassign" {
    try compiler_test_generic_str(
        multi_assign_reassign,
        "multi_assign_reassign",
    );
}

test "compiler assign_block_empty" {
    try compiler_test_generic_str(
        assign_block_empty,
        "assign_block_empty",
    );
}

test "compiler assign_block" {
    try compiler_test_generic_str(
        assign_block,
        "assign_block",
    );
}

test "compiler assign_block2" {
    try compiler_test_generic_str(
        assign_block2,
        "assign_block2",
    );
}

const section = "section";
const dtext = ".text";
const ddata = ".data";
const global = "global";
const qword_ptr = "qword"; // "qword ptr" in as
const dword_ptr = "dword"; // "dword ptr" in as
const byte_ptr = "byte"; // "dword ptr" in as

fn compiler_test_generic_str(test_str: []const u8, test_name: []const u8) !void {
    std.log.info("=> starting compiler test: {s}\n", .{test_name});
    std.log.info("=> test str: {s}\n", .{test_str});

    var memory = std.heap.DebugAllocator(.{}).init;
    // defer _ = memory.deinit();
    const alloc = memory.allocator();
    // const alloc = std.testing.allocator;
    var ctx = context.new_context(test_str);
    var cmp = try new_compiler(alloc, &ctx);
    defer cmp.deinit();

    _ = try cmp.compile();
    std.log.info("{s}", .{cmp.asm_str.items});

    const fpath = "tmp";

    std.log.info("PATH: {s}\n\n\n\n", .{fpath});

    const f = try std.fs.cwd().createFile(fpath ++ ".asm", .{});
    defer std.fs.cwd().deleteFile(fpath) catch {};
    defer std.fs.cwd().deleteFile(fpath ++ ".asm") catch {};

    try f.writeAll(cmp.asm_str.items);
    try run(fpath, alloc);
    std.log.info("=> finished compiler test: {s}\n", .{test_name});
}

fn run(fpath: []const u8, alloc: std.mem.Allocator) !void {
    var buf1: [20]u8 = undefined;
    var buf2: [20]u8 = undefined;
    var buf3: [20]u8 = undefined;

    const f_asm = try std.fmt.bufPrint(&buf1, "{s}.asm", .{fpath});
    const f_o = try std.fmt.bufPrint(&buf2, "{s}.o", .{fpath});
    const f_out = try std.fmt.bufPrint(&buf3, "./{s}", .{fpath});

    var child = std.process.Child.init(&.{ "nasm", "-f", "elf64", "-o", f_o, f_asm }, alloc);
    var term = try child.spawnAndWait();
    switch (term) {
        .Exited => |e| if (e != 0) std.log.err("nasm exit err\n", .{}),
        .Signal => std.log.err("nasm sig err\n", .{}),
        else => {},
    }

    child = std.process.Child.init(&.{ "gcc", "-no-pie", "-s", "-o", fpath, f_o }, alloc);
    term = try child.spawnAndWait();
    switch (term) {
        .Exited => |e| if (e != 0) std.log.err("Err", .{}),
        .Signal => std.log.err("Err", .{}),
        else => {},
    }

    child = std.process.Child.init(&.{f_out}, alloc);
    term = try child.spawnAndWait();
    switch (term) {
        .Exited => |e| if (e != 0) std.log.err("prog exit err\n", .{}),
        .Signal => std.log.err("prog sig err\n", .{}),
        else => {},
    }

    // try std.process.Child.init(&.{ "gcc -s -o", abs_path, abs_path, ".o" }, alloc).wait();
}

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
