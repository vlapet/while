const std = @import("std");
const context = @import("context.zig");

const Self = @This();

// pub const function = struct {
//     ident: []const u8,
//     params: []anytype,
// };

pub const BasicType = union(enum) {
    val_lit: []const u8, // TODO: rename to val_str
    val_num: f64, // For now all numbers are f64
    val_bool: bool, //u8, // Can only allocate 1 byte at a time
    val_char: u8,
    // val_misc: ANY, // this is to allow structs and custom data types
};

pub const Block = struct {
    stmts: ?*Statements,
    expr: ?*Expr,
};

pub const AssignReassign = union(enum) {
    assign: Assign,
    reassign: ReAssign,
};

const field = struct {
    ident: []const u8,
    type: type,
};

const @"struct" = struct {
    // fields: std.AutoArrayHashMap(comptime K: type, comptime V: type)
};

pub const Var = struct {
    is_const: bool,
    ident: []const u8,
};

pub const AssignInner = union(enum) {
    expr: Expr,
    // block: Block, Move to Expr
};

const ReAssign = struct {
    ident: []const u8,
    assign: AssignInner,
};

pub const Assign = struct {
    @"var": Var,
    assign: AssignInner,
};

pub const Expr = union(enum) {
    // expr: Expr,
    //if: TODO!,
    basic_var: BasicType,
    block: Block,
    // bin_op: BinOperation,
    // uni_op: UniOperation,

    // @"var": []const u8,
};

const Exprs = struct {
    expr: Expr,
    exprs: ?*Exprs,
};

pub const BinOperation = struct {
    tag: BinOpTag,
    lhs: Expr,
    rhs: Expr,
};

const UniOperation = struct {
    tag: UniOpTag,
    expr: Expr,
};

const UniOpTag = enum {
    not,
};

const BinOpTag = enum {
    add,
    sub,
    mul,
    div,
    mod,
    floor,
    ceil,
    exp,
    @"or",
    @"and",
    xor,
    lshift,
    rshift,
};

pub const Statement = union(enum) {
    assign: Assign,
    reassign: ReAssign,
    //@"if": If
    // loop,
    block: Block,
    // Return
};

pub const Statements = struct {
    statement: Statement,
    statements: ?*const Statements,
};

pub const Ast = union(enum) {
    @"var": Var,
    expr: Expr,
    // exprs: Exprs,
    stmt: Statement,
    stmts: Statements,
    bin_op: BinOperation,
    uni_op: UniOperation,
    // semicolon,
    // assign: Assign,
    eof,
    // root: void,
};

pub fn eval_type(ast: @This().Ast) !type {
    switch (ast) {
        .expr => |e| switch (e) {
            .val_lit => []const u8,
            .val_bool => bool,
            .val_num => f64,
        },
        else => error.UnimplementedTypeCheck,
    }
}

pub fn check_ast_tag(expected: Ast, actual: Ast) !void {
    if (std.meta.activeTag(expected) != std.meta.activeTag(actual)) {
        std.log.err("Ast didn't match: {}\t{}", .{ expected, actual });
        return error.UnexpectedAst;
    }
}

pub fn check_ast(expected: Ast, actual: Ast) !void {
    const err = error.UnexpectedAst;
    try check_ast_tag(expected, actual);

    switch (expected) {
        .@"var" => |v| {
            // if (actual.@"var".ident != v.ident) {
            if (!std.mem.eql(u8, actual.@"var".ident, v.ident)) {
                std.log.err("Expected var ident to be equal", .{});
                return err;
            }
            if (actual.@"var".is_const != v.is_const) {
                std.log.err("Expected var ident to be {s}", .{if (v.is_const) "const" else "var"});
                return err;
            }
        },
        .assign => |a| {
            try check_ast(Ast{ .@"var" = a.@"var" }, Ast{ .@"var" = actual.assign.@"var" });
            try check_ast(Ast{ .expr = a.expr }, Ast{ .expr = actual.assign.expr });
        },
        .bin_op => |b| {
            _ = b;
            // try check_ast_tag(b.tag, actual.bin_op.tag);
            return error.UnimplementedAst; // TODO
        },
        // .semicolon => return check_ast_tag(expected, actual),
        .expr => |e| {
            const A = std.meta.activeTag(actual.expr);
            const E = std.meta.activeTag(e);

            if (A != E) {
                return err;
            }

            switch (e) {
                .val_lit => |v| {
                    if (!std.mem.eql(u8, v, actual.expr.val_lit)) {
                        return err;
                    }
                },
                .val_num => |v| {
                    if (v != actual.expr.val_num) {
                        return err;
                    }
                },
            }

            // return err;
        },
        .eof => {
            // std.debug.print("EOF", .{});
        },
        else => |a| {
            std.log.err("Got unimplemeted ast:\n", .{});
            try print_ast(a);
            return error.UnimplemetedAst;
        },
    }
}

pub fn print_ast(ast: Ast) !void {
    switch (ast) {
        .@"var" => |v| {
            std.debug.print(
                "var -> ident: {s}\tis_const: {}\t",
                .{ v.ident, v.is_const },
            );
        },

        // .assign => |a| {
        //     std.debug.print("assign ->", .{});
        //     try print_ast(Ast{ .@"var" = a.@"var" });
        //     // try print_ast(Ast{ .expr = a.expr });
        //     switch (a.assign) {
        //         .expr => |e| try print_ast(Ast{ .expr = e }),
        //     }
        // },

        .bin_op => |b| {
            std.debug.print("bin_op: {s}", .{@tagName(b.tag)});
            try print_ast(Ast{ .expr = b.lhs });
            try print_ast(Ast{ .expr = b.rhs });
        },
        .expr => |e| {
            switch (e) {
                .basic_var => |b| {
                    std.debug.print("basic_var:\t", .{});
                    switch (b) {
                        .val_lit => |v| std.debug.print("val_lit: {s}\n", .{v}),
                        .val_num => |v| std.debug.print("val_num: {}\n", .{v}),
                        .val_bool => |v| std.debug.print("val_bool: {any}\n", .{v}),
                        .val_char => |v| std.debug.print("val_char: {c}\n", .{v}),
                    }
                },
                .block => |b| {
                    std.debug.print("BLOCK: \n", .{});
                    if (b.stmts) |s| try print_ast(Ast{ .stmts = s.* }) else std.debug.print("stmts: NULL\n", .{});
                    if (b.expr) |ex| try print_ast(Ast{ .expr = ex.* }) else std.debug.print("expr: NULL\n", .{});
                    std.debug.print("ENDBLOCK: \n", .{});
                },
            }
        },
        // .exprs => |e| {
        //     try print_ast(Ast{ .expr = e.expr });
        //     if (e.exprs) |ex| {
        //         try print_ast(Ast{ .exprs = ex.* });
        //     }
        // },
        // .semicolon => std.debug.print(";", .{}),
        .uni_op => |u| {
            std.debug.print("uni_op: {s}", .{@tagName(u.tag)});
            try print_ast(Ast{ .expr = u.expr });
        },
        .eof => std.debug.print("EOF\n", .{}),
        .stmt => |s| {
            switch (s) {
                .assign => |a| {
                    std.debug.print("assign ->", .{});
                    try print_ast(Ast{ .@"var" = a.@"var" });
                    // try print_ast(Ast{ .expr = a.expr });
                    switch (a.assign) {
                        .expr => |e| try print_ast(Ast{ .expr = e }),
                    }
                },
                .reassign => |r| {
                    std.debug.print("reassign -> ident -> {s}\t", .{r.ident});
                    // try print_ast(Ast{ .expr = a.expr });
                    switch (r.assign) {
                        .expr => |e| try print_ast(Ast{ .expr = e }),
                    }
                },
                .block => std.debug.print("BLOCK: \t", .{}),
                // inline else => |i, t| try print_ast(@unionInit(Ast, @tagName(t), i)),
            }
        },
        .stmts => |s| {
            try print_ast(Ast{ .stmt = s.statement });

            if (s.statements) |s_i|
                try print_ast(Ast{ .stmts = s_i.* })
            else
                std.debug.print("STMTSNULL\n", .{});
        },
    }
}

test "ast" {
    // asm volatile ("");
}
