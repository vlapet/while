const std = @import("std");
const Context = @import("context.zig");
const Tok = @import("token.zig");
const Token = Tok.Token;
const Ast = @import("ast.zig");
const AstIn = Ast.Ast;

// const BUF_SZ = 1024; // Ast size is limited to this many bytes
// var buf = [BUF_SZ]u8;
//
// const fixed_buf = std.heap.FixedBufferAllocator.init(buf);
// const allocator = fixed_buf.allocator();

const parse_error = error{
    UnmatchedToken,
};

pub fn parse_stmts(ctx: *Context, alloc: std.mem.Allocator) !Ast.Ast {
    std.log.debug("parse stmts\n", .{});
    const stmt = try parse_stmt(ctx, alloc);

    switch (stmt) {
        .eof => return .eof,
        else => {},
    }

    const sem = try ctx.readToken();

    switch (sem) {
        .semicolon => {},
        else => |e| {
            std.log.err("UNREACHABLE! Token: {any}\n", .{e});

            unreachable;
        },
    }

    const stmts = try alloc.create(AstIn);
    // defer alloc.destroy(stmts); <- Investigate

    // const stmts = try parse_stmts(ctx, alloc);
    stmts.* = try parse_stmts(ctx, alloc);

    const stmts_res = switch (stmts.*) {
        .eof => null,
        .stmts => &stmts.stmts,
        else => unreachable,
    };

    return AstIn{ .stmts = .{ .statement = stmt.stmt, .statements = stmts_res } };
}

pub fn parse_stmt(ctx: *Context, alloc: std.mem.Allocator) !Ast.Ast {
    std.log.debug("parse stmt\n", .{});
    const tok = try ctx.readToken();

    return parse_stmt_paramed(ctx, alloc, tok);
}

pub fn parse_stmt_paramed(ctx: *Context, alloc: std.mem.Allocator, tok: Token) !Ast.Ast {
    return switch (tok) {
        .@"const" => parse_var(ctx, alloc, true),
        .@"var" => parse_var(ctx, alloc, false),
        .eof => AstIn.eof,
        .literal => |lit| {
            const assign = try ctx.readToken();

            switch (assign) {
                .assign => {},
                else => return error.ExpectedAssign,
            }
            //TODO: Handle blocks
            const expr = try parse_expression(ctx, alloc);

            return Ast.Ast{
                .stmt = .{
                    .reassign = .{
                        .ident = lit,
                        .assign = .{ .expr = expr.expr },
                    },
                },
            };
        },
        else => |e| {
            std.log.err("UNREACHABLE! Token: {any}\n", .{e});

            unreachable;
        },
    };
}

pub fn parse_var(ctx: *Context, alloc: std.mem.Allocator, is_const: bool) !Ast.Ast {
    const lit = try ctx.readToken();

    switch (lit) {
        .literal => {},
        else => return error.ExpectedLit,
    }

    const assign_tok = try ctx.readToken();

    try check_tok(assign_tok, Token.assign);
    const b = try ctx.peekByte();

    const assign = if (b.* == '{')
        unreachable // TODO
        // parse_block(...);
    else
        try parse_expression(ctx, alloc);

    return Ast.Ast{
        .stmt = .{
            .assign = .{
                .@"var" = .{ .ident = lit.literal, .is_const = is_const },
                .assign = switch (assign) {
                    .expr => |e| .{ .expr = e },
                    else => unreachable,
                },
            },
        },
    };
}

pub fn parse_expression(ctx: *Context, alloc: std.mem.Allocator) !Ast.Ast {
    // _ = alloc;
    const tok = try ctx.readToken();

    return parse_expression_paramed(ctx, alloc, tok);
}

pub fn parse_expression_paramed(ctx: *Context, alloc: std.mem.Allocator, tok: Token) !Ast.Ast {
    return switch (tok) {
        .@"if" => unreachable,
        // .literal => |l| Ast.Ast{ .expr = .{ .val_lit = l } },
        .literal => error.UnexpectedLiteral,
        .quoted_literal => |q| Ast.Ast{ .expr = .{ .basic_var = .{ .val_lit = q } } },
        .number => |n| Ast.Ast{ .expr = .{ .basic_var = .{ .val_num = n } } },
        .true => Ast.Ast{ .expr = .{ .basic_var = .{ .val_bool = true } } },
        .false => Ast.Ast{ .expr = .{ .basic_var = .{ .val_bool = false } } },
        // .lbrace => unreachable,
        .function => unreachable,
        .@"return" => unreachable,
        .lbrace => return try parse_block(ctx, alloc),

        else => unreachable,
    };
}

const blk_err = error{
    OutOfBounds,
    IncorrectNumberFormat,
    TokenNotFound,
    OutOfMemory,
    UnexpectedLiteral,
    ExpectedAssign,
    ExpectedLit,
};

// const stmt_or_expr = union(enum) {
//     stmt: Ast.Statement,
//     expr: Ast.Expr,
// };

pub fn parse_block(ctx: *Context, alloc: std.mem.Allocator) blk_err!Ast.Ast {
    const expr_or_stmt_tok = try ctx.readToken();
    const res = switch (expr_or_stmt_tok) {
        .@"const" => try parse_var(ctx, alloc, true),
        .@"var" => try parse_var(ctx, alloc, true),
        .number => |n| Ast.Ast{ .expr = .{
            .basic_var = .{ .val_num = n },
        } },
        .lbrace => bk: {
            const ast_blk = try parse_block(ctx, alloc);
            break :bk Ast.Ast{ .expr = .{
                .block = ast_blk.expr.block,
            } };
        },
        .rbrace => { // block MUST be empty
            return Ast.Ast{ .expr = .{
                .block = .{
                    .expr = null,
                    .stmts = null,
                },
            } };
        },

        .true, .false => unreachable,
        .@"if", .@"return", .@"while" => unreachable,
        .semicolon => std.debug.panic("Semicolon is invalid here\n", .{}),
        else => |e| std.debug.panic("UNREACHABLE IN BLOCK: {s}", .{@tagName(e)}),
    };

    const tok_end = try ctx.readToken();
    const tok_end_stmt_check = try ctx.peekToken();

    std.log.info("expr_or_stmt_tok: {s}\ttok_end: {s}\ttok_end_stmt_check: {s}\n", .{ @tagName(expr_or_stmt_tok), @tagName(tok_end), @tagName(tok_end_stmt_check) });

    // Handle end of block
    switch (tok_end) {
        .rbrace => { // expr_or_stmt is expr
            // std.debug.panic("comptime format: []const u8", .{});
            const expr = try alloc.create(Ast.Expr);
            expr.* = res.expr;
            std.debug.print("TAGNAME: {s}\n", .{@tagName(res.expr)});
            // @memcpy(expr[0..1], (&res.expr)[0..1]);
            std.debug.print("TAGNAMEEXPR: {s}\n", .{@tagName(expr.*)});
            // @unionInit(Ast.Expr, @tagName(res.expr), res.expr);
            return Ast.Ast{ .expr = .{ .block = .{
                .expr = expr,
                .stmts = null,
            } } };
        },
        .semicolon => {
            switch (tok_end_stmt_check) {
                .rbrace => { // expr_or_stmt is end stmt with no end expr
                    _ = try ctx.readToken(); // discard rbrace
                    const stmts = try alloc.create(Ast.Statements);
                    const stmt = res.stmt;

                    stmts.* = Ast.Statements{
                        .statement = stmt,
                        .statements = null,
                    };

                    return Ast.Ast{ .expr = .{ .block = .{
                        .expr = null,
                        .stmts = stmts,
                    } } };
                },
                else => {},
            }
        },
        .eof => unreachable,
        else => {},
    }

    const block = try parse_block(ctx, alloc);

    const stmts = try alloc.create(Ast.Statements);

    // Here we will convert block expr to block stmt
    // const stmt = res.stmt;
    const stmt = switch (res) {
        .stmt => |s| s,
        .expr => |e| switch (e) {
            .block => |b| Ast.Statement{ .block = b },
            else => unreachable,
        },
        else => unreachable,
    };

    stmts.* = Ast.Statements{
        .statement = stmt,
        .statements = block.expr.block.stmts,
    };

    return Ast.Ast{ .expr = .{ .block = .{
        .expr = block.expr.block.expr,
        .stmts = stmts,
    } } };
}

pub fn parse_function(ctx: *Context, alloc: std.mem.Allocator) !Ast.Ast {
    _ = ctx;
    _ = alloc;
    undefined;
}

// pub fn parse_expressions(ctx: *Context, alloc: std.mem.Allocator) !Ast.Ast {
//     // _ = alloc;

//     const expr = switch (try ctx.readToken()) {
//         .eof => unreachable,
//         _ => try parse_expression(ctx, alloc),
//     };

//     const exprs = try parse_expressions(ctx, alloc);

//     return Ast.Ast.exprs{ .expr = expr, .exprs = exprs };
// }

pub fn check_tok(actual: Token, expected: Token) !void {
    if (std.meta.activeTag(actual) != std.meta.activeTag(expected)) {
        switch (actual) {
            .number => std.debug.panic("Unexpected Token: {}\tExpected: {}", .{ actual, expected }),
            inline else => |s| {
                if (@TypeOf(s) == void) {
                    std.debug.panic("Unexpected Token: {}\tExpected: {}", .{ actual, expected });
                } else {
                    std.debug.panic("Unexpected Token: {s}: {s}\tExpected: {}", .{ @tagName(actual), s, expected });
                }
            },
        }
    }
}

const v1 = "";
const v2 = "const x = 3;";
const v3 = "const x = 45; var y = true; var z = 1.234; y = false; const a = \"abcd\";";

const reassign = "const x = 3; x = 4;";

const assign_block_empty = "const x = { };";
const assign_block = "const x = { 3 };";
const assign_block2 = "const x = { const y = 4; 3 };";
const assign_nested = "const x = { { 3 } };";
const assign_nested_seq = "const x = { {}; { 3 } };";

const r1 = "x : 3;"; // no - syntax error
const r2 = "x = 3;"; // no - assigned variable

test "parse v1" {
    try parse_test_generic_str(
        v1,
        "v1",
    );
}

test "parse v2" {
    try parse_test_generic_str(
        v2,
        "v2",
    );
}
test "parse v3" {
    try parse_test_generic_str(
        v3,
        "v3",
    );
}

test "parse reassign" {
    try parse_test_generic_str(
        reassign,
        "reassign",
    );
}

test "parse assign_block_empty" {
    try parse_test_generic_str(
        assign_block_empty,
        "assign_block_empty",
    );
}

test "parse assign_block" {
    try parse_test_generic_str(
        assign_block,
        "assign_block",
    );
}

test "parse assign_block2" {
    try parse_test_generic_str(
        assign_block2,
        "assign_block2",
    );
}

test "parse assign_nested" {
    try parse_test_generic_str(
        assign_nested,
        "assign_nested",
    );
}

test "parse assign_nested_seq" {
    try parse_test_generic_str(
        assign_nested_seq,
        "assign_nested_seq",
    );
}

inline fn parse_test_generic_str(test_str: []const u8, test_name: []const u8) !void {
    std.log.info("=> starting parse test: {s}\n", .{test_name});
    std.log.info("=> test str: {s}\n", .{test_str});

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var ctx = Context.new_context(assign_nested_seq);
    const ast = try parse_stmts(&ctx, alloc);
    // _ = ast;
    try Ast.print_ast(ast);

    std.log.info("=> finished parse test: {s}\n", .{test_name});
}
