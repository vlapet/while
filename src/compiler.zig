/// The compiler lazily interprets the file
const std = @import("std");
const context = @import("context.zig");
const State = @import("state.zig");
const Ast = @import("ast.zig");
const AstIn = Ast.Ast;
const Parse = @import("parse.zig");

const Self = @This();
const var_map = std.AutoArrayHashMapUnmanaged([]const u8, u32);

const StackFrame = struct {
    vars: var_map,
    var_ptr: u32 = 0,
};

const var_stack = std.ArrayListUnmanaged(StackFrame);

// state: State,
ctx: *context,
alloc: std.mem.Allocator,
asm_str: std.ArrayListUnmanaged(u8),
vars: var_stack,
buf: [50]u8,

// main: ?*Ast., // TODO: TBD

// pub fn eval_type(_: *Self, ast: AstIn) !type {} // use zig's underlying type system

/// All the assembly is in self.ast_str
fn gen_asm(self: *Self, ast: AstIn) !void {
    switch (ast) {
        .stmts => |s| {
            _ = try gen_asm(self, AstIn{ .stmt = s.statement });
            if (s.statements) |stmts|
                _ = try gen_asm(self, AstIn{ .stmts = stmts.* });
        },
        .stmt => |s| switch (s) {
            .assign => |a| {
                _ = try self.gen_asm_assign(a);
            },
            // .reassign => |r| {
            //     const loc = self.vars.getLast().vars.get(r.ident).?;

            // },
            else => return error.Undefined,
        },
        else => return error.Unexpected,
    }

    // return error.Todo;
}

fn gen_asm_assign(self: *Self, assign: Ast.Assign) ![]const u8 {
    // const ident = assign.@"var".ident;
    // const is_const = assign.@"var".is_const;
    // const curr_stack = &self.vars.getLast();
    const Res = struct { size: u32, str_len: usize };
    const curr_stack = &self.vars.items[self.vars.items.len - 1];

    // var buf: [100]u8 = std.mem.zeroes([100]u8);
    // const buf = try self.alloc.alloc(u8, 50);

    std.log.debug("gen_asm_assign ptr before: {}\n", .{curr_stack.var_ptr});

    const res = asm_lbl: {
        _ = switch (assign.assign) {
            .expr => |e| {
                _ = switch (e) {
                    // .val_bool => |_| undefined,
                    inline .val_num, .val_bool, .val_char => |n| {
                        const size = switch (@TypeOf(n)) {
                            bool => 8,
                            else => |t| @bitSizeOf(t),
                        };
                        const ptr = curr_stack.*.var_ptr;

                        const ptr_type_str = switch (@TypeOf(n)) {
                            u8, bool => "byte ptr",
                            f64 => "qword ptr",
                            else => return error.Undefined,
                        };

                        const s = try std.fmt.bufPrint(&self.buf, "mov {s} [rbp-{}], {}", .{ ptr_type_str, ptr + size, n });
                        break :asm_lbl Res{ .size = size, .str_len = s.len };

                        // break :asm_lbl Asm.mov ++ Asm.dword_ptr ++ "[" ++ Asm.rbp ++ "-" ++ ptr ++ "], " ++ n;
                    },
                    else => undefined,
                };
                // std.log.debug("{s}", .{asm_str});
            },
        };

        return error.undefined;
    };

    // try self.asm_str.appendSlice(self.alloc, asm_str);

    // std.log.debug("gen_asm_assign ptr mid: {}\n", .{curr_stack.var_ptr});
    curr_stack.var_ptr += res.size;
    std.log.debug("gen_asm_assign ptr after: {}\n", .{curr_stack.var_ptr});

    std.log.info("gen_asm_assign: {s}\n", .{self.buf[0..res.str_len]});

    // _ = ident;
    // _ = is_const;

    return self.buf[0..res.str_len];
}

pub fn new_compiler(alloc: std.mem.Allocator, ctx: *context) !Self {
    std.log.warn("There are memory leaks, need to use arena allocator\n", .{});
    var arr = try std.ArrayListUnmanaged(StackFrame).initCapacity(alloc, 64);
    const stk = StackFrame{ .var_ptr = 0, .vars = var_map{} };
    try arr.append(alloc, stk);

    return .{
        .alloc = alloc,
        .ctx = ctx,
        // .vars = try std.ArrayListUnmanaged(StackFrame).initCapacity(alloc, 64),
        .vars = arr,
        .asm_str = try std.ArrayListUnmanaged(u8).initCapacity(alloc, 64),
        .buf = std.mem.zeroes([50]u8),
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
const multi_assign = "const x = 45; var y = true; var z = 1.234;";
// const v3 = "const x = 45; var y = true; var z = 1.234; y = false; const a = \"abcd\";";

test "compiler bool" {
    var memory = std.heap.DebugAllocator(.{}).init;
    // defer _ = memory.deinit();
    const alloc = memory.allocator();

    var ctx = context.new_context(bool_str);

    var cmp = try new_compiler(alloc, &ctx);
    defer cmp.deinit();

    _ = try cmp.compile();

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
    std.log.info("finished compiler test num", .{});
}

test "compiler multi_assign" {
    var memory = std.heap.DebugAllocator(.{}).init;
    // defer _ = memory.deinit();
    const alloc = memory.allocator();
    // const alloc = std.testing.allocator;
    var ctx = context.new_context(multi_assign);
    var cmp = try new_compiler(alloc, &ctx);
    defer cmp.deinit();

    _ = try cmp.compile();
    std.log.info("finished compiler test multi_assign", .{});
}

// const AsmMap = std.StaticStringMap(Token).initComptime();

const Asm = if (true) struct {
    eax: []const u8 = "eax",
    rax: []const u8 = "rax",
    jmp: []const u8 = "jmp",
    mov: []const u8 = "mov",
    cmp: []const u8 = "cmp",
    // rex_pref : []const u8= "",
    dword_ptr: []const u8 = "dword ptr",
    qword_ptr: []const u8 = "qword ptr",
    rbp: []const u8 = "rbp",
    // push = "push",
    // pop = "pop",
    // ret = "ret",
    // rsp = "rsp",
}{} else @compileError("Need to implement assembly instruction");

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
