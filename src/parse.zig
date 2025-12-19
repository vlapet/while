const std = @import("std");
const Context = @import("context.zig");
const Tok = @import("token.zig");
const Token = Tok.Token;
const Types = @import("types.zig");
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

    const skip_semicolon = switch (stmt.stmt) {
        .@"if" => true,
        .block => |b| if (b.expr) |_| std.debug.panic("block cannot have expr return as statement block\n", .{}) else true,
        .loop => true,
        else => false,
    };

    const sem = try ctx.readToken();

    switch (sem) {
        .semicolon => {},
        else => |e| {
            if (!skip_semicolon) {
                std.log.err("UNREACHABLE! Token: {any}\n", .{e});

                unreachable;
            }
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
                        .assign = expr.expr,
                    },
                },
            };
        },
        .lbrace => try parse_block_opt(ctx, alloc, .stmt),
        .@"if" => try parse_if_opt(ctx, alloc, .stmt),
        .loop => try parse_loop_opt(ctx, alloc, .stmt),
        else => |e| {
            std.log.err("UNREACHABLE! Token: {any}\n", .{e});

            unreachable;
        },
    };
}

pub fn parse_type(ctx: *Context, alloc: std.mem.Allocator) !Types.SysType {
    const tok = try ctx.readToken();

    _ = alloc;

    return switch (tok) {
        .void => Types.SysType.Void,
        .null => Types.SysType.Null,
        .num => Types.SysType{ .Float = Ast.FLOAT_CONST }, // TDB
        else => std.debug.panic("Undefined type\n", .{}),
    };
}

pub fn parse_var(ctx: *Context, alloc: std.mem.Allocator, is_const: bool) !Ast.Ast {
    const lit = try ctx.readToken();

    switch (lit) {
        .literal => {},
        else => return error.ExpectedLit,
    }

    const type_specify = try ctx.peekToken();
    const var_type: Ast.VarTypeSpecified = switch (type_specify) {
        .colon => blk: {
            _ = try ctx.readToken();
            break :blk .{ .specified = try parse_type(ctx, alloc) };
        },
        .assign => .infer,
        else => return error.UnmatchedToken,
    };

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
                .@"var" = .{
                    .ident = lit.literal,
                    .is_const = is_const,
                    .type = var_type,
                },

                .assign = switch (assign) {
                    // .expr => |e| .{ .expr = e },
                    .expr => |e| e,
                    else => unreachable,
                },
            },
        },
    };
}

pub fn parse_expression(ctx: *Context, alloc: std.mem.Allocator) !Ast.Ast {
    // _ = alloc;
    const tok = try ctx.readToken();
    std.log.debug("Expr Token: {any}\n", .{tok});

    return parse_expression_paramed(ctx, alloc, tok);
}

pub fn parse_expression_paramed(ctx: *Context, alloc: std.mem.Allocator, tok: Token) !Ast.Ast {
    return switch (tok) {
        .@"if" => i: {
            // const if_expr = try parse_if(ctx, alloc);
            // break :i Ast.Ast{ .expr = .{ .@"if" = if_expr } };
            break :i try parse_if_opt(ctx, alloc, .expr);
        },
        // .literal => |l| Ast.Ast{ .expr = .{ .val_lit = l } },
        .literal => error.UnexpectedLiteral,
        .quoted_literal => |q| Ast.Ast{ .expr = .{ .basic_var = .{ .val_lit = q } } },
        .number => |n| Ast.Ast{ .expr = .{ .basic_var = .{ .val_num = n } } },
        .true => Ast.Ast{ .expr = .{ .basic_var = .{ .val_bool = true } } },
        .false => Ast.Ast{ .expr = .{ .basic_var = .{ .val_bool = false } } },
        // .lbrace => unreachable,
        .function => unreachable,
        .@"return" => unreachable,
        .lbrace => return try parse_block_opt(ctx, alloc, .expr),
        .loop => try parse_loop_opt(ctx, alloc, .expr),
        else => |e| {
            std.log.err("UNREACHABLE! Token: {any}\n", .{e});

            unreachable;
        },
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

// pub fn parse_block(ctx: *Context, alloc: std.mem.Allocator) blk_err!Ast.Ast {
pub fn parse_block(ctx: *Context, alloc: std.mem.Allocator) anyerror!Ast.Block {
    std.log.debug("Start parsing block \n", .{});
    defer std.log.debug("finish parsing block\n", .{});

    const expr_or_stmt_tok = try ctx.readToken();
    const res = switch (expr_or_stmt_tok) {
        .@"const" => try parse_var(ctx, alloc, true),
        .@"var" => try parse_var(ctx, alloc, true),
        .number => |n| Ast.Ast{ .expr = .{
            .basic_var = .{ .val_num = n },
        } },
        .lbrace => try parse_block_opt(ctx, alloc, .expr),
        .rbrace => { // block MUST be empty
            std.log.debug("Empty Block\n", .{});

            return Ast.Block{
                .expr = null,
                .stmts = null,
            };
        },

        .true, .false => unreachable,
        .@"if", .@"return", .@"while" => unreachable,
        .semicolon => std.debug.panic("Semicolon is invalid here\n", .{}),
        .@"break" => b: {
            const peek_tok = try ctx.peekToken();
            std.log.debug("block break - peek_tok: {any}\n", .{peek_tok});

            const expr_ast = switch (peek_tok) {
                .semicolon => null,
                else => try parse_expression(ctx, alloc),
            };

            const expr_res = if (expr_ast) |ea| ea.expr else null;
            break :b Ast.Ast{ .stmt = .{ .@"break" = .{ .expr = expr_res } } };
        },
        .loop => try parse_loop_opt(ctx, alloc, .stmt),
        // .literal => try parse
        // else => |e| std.debug.panic("UNREACHABLE IN BLOCK: {s}", .{@tagName(e)}),
        else => |e| std.debug.panic("UNREACHABLE IN BLOCK: {s}", .{@tagName(e)}),
    };

    // const tok_end = try ctx.readToken();
    const tok_end = switch (expr_or_stmt_tok) {
        .loop, .@"if" => // loops and if do not have trailing semicolon
        // try ctx.peekToken(),
        Token.semicolon, // pretend there is a semicolon after the loop
        else => try ctx.readToken(),
    };
    const tok_end_stmt_check = try ctx.peekToken();

    std.log.info("expr_or_stmt_tok: {s}\ttok_end: {s}\ttok_end_stmt_check: {s}\n", .{ @tagName(expr_or_stmt_tok), @tagName(tok_end), @tagName(tok_end_stmt_check) });

    // Handle end of block
    switch (tok_end) {
        .rbrace => { // expr_or_stmt is expr
            // std.debug.panic("comptime format: []const u8", .{});
            const expr = try alloc.create(Ast.Expr);
            // expr.* = res.expr;
            expr.* = switch (res) {
                .expr => |e| e,
                .stmt => |s| switch (s) {
                    // Loops do not need a semicolon after block
                    .loop => |l| Ast.Expr{ .loop = l },
                    else => unreachable,
                },
                else => unreachable,
            };

            // std.debug.print("TAGNAME: {s}\n", .{@tagName(res.expr)});
            std.debug.print("TAGNAMEEXPR: {s}\n", .{@tagName(expr.*)});

            return Ast.Block{
                .expr = expr,
                .stmts = null,
            };
        },
        .semicolon => {
            switch (tok_end_stmt_check) {
                .rbrace => { // expr_or_stmt is end stmt with no end expr
                    _ = try ctx.readToken(); // discard rbrace
                    const stmts = try alloc.create(Ast.Statements);
                    // const stmt = res.stmt;
                    const stmt = switch (res) {
                        .stmt => |st| st,
                        .expr => |ex| switch (ex) {
                            .basic_var => std.debug.panic("BasicVar cannot be parsed as a statement\n", .{}),
                            // .@"if" => |i| Ast.Statement{ .@"if" = i },
                            .block => |b| Ast.Statement{ .block = b },
                            else => unreachable,
                        },
                        else => unreachable,
                    };

                    stmts.* = Ast.Statements{
                        .statement = stmt,
                        .statements = null,
                    };

                    return Ast.Block{
                        .expr = null,
                        .stmts = stmts,
                    };
                },
                else => {},
            }
        },
        .eof => unreachable,
        else => {},
    }

    // const block = try parse_block(ctx, alloc, .stmt);
    const block = try parse_block_opt(ctx, alloc, .expr);

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

    return Ast.Block{
        .expr = block.expr.block.expr,
        .stmts = stmts,
    };
}

pub fn parse_block_opt(ctx: *Context, alloc: std.mem.Allocator, opt: enum { stmt, expr }) anyerror!Ast.Ast {
    std.log.debug("Start parsing block opt - as: {s}\n", .{switch (opt) {
        .stmt => "stmt",
        .expr => "expr",
    }});
    defer std.log.debug("finish parsing block opt\n", .{});

    const block = try parse_block(ctx, alloc);
    return switch (opt) {
        .expr => Ast.Ast{ .expr = .{ .block = block } },
        .stmt => Ast.Ast{ .stmt = .{ .block = block } },
    };
}

// Examples:
// if true {const x = 5} else {const y = 4;}
// const x = if true {3} else {4};
pub fn parse_if(ctx: *Context, alloc: std.mem.Allocator) anyerror!Ast.If {
    const cond_expr = try alloc.create(Ast.Expr);
    const if_expr = try alloc.create(Ast.Expr);
    var else_expr: ?*Ast.Expr = null;

    const cond_parse = try parse_expression(ctx, alloc);
    cond_expr.* = cond_parse.expr;

    const lbrace = try ctx.readToken();
    try check_tok(lbrace, Token.lbrace);

    const if_parse = try parse_block_opt(ctx, alloc, .expr);
    if_expr.* = if_parse.expr;

    const check_else_tok = try ctx.peekToken();

    switch (check_else_tok) {
        .@"else" => {
            _ = try ctx.readToken(); // discard else token
            else_expr = try alloc.create(Ast.Expr);
            const if_lbrace_chk = try ctx.readToken();
            const else_parse = switch (if_lbrace_chk) {
                .lbrace => try parse_block_opt(ctx, alloc, .expr),
                .@"if" => try parse_if_opt(ctx, alloc, .expr),
                inline else => |_, t| std.debug.panic("Unreachable token found: {}\n", .{t}),
            };
            else_expr.?.* = else_parse.expr;
        },
        else => {},
    }

    return Ast.If{ .cond_expr = cond_expr, .if_expr = if_expr, .else_expr = else_expr };
}

pub fn parse_if_opt(ctx: *Context, alloc: std.mem.Allocator, opt: enum { stmt, expr }) anyerror!Ast.Ast {
    const i = try parse_if(ctx, alloc);

    return switch (opt) {
        .expr => Ast.Ast{ .expr = .{ .@"if" = i } },
        .stmt => Ast.Ast{ .stmt = .{ .@"if" = i } },
    };
}

pub fn parse_loop(ctx: *Context, alloc: std.mem.Allocator) !Ast.Loop {
    const lbrace = try ctx.readToken();
    try check_tok(lbrace, Token.lbrace);

    const blk = try parse_block(ctx, alloc);
    return Ast.Loop{ .block = blk };
}

pub fn parse_loop_opt(ctx: *Context, alloc: std.mem.Allocator, opt: enum { stmt, expr }) anyerror!Ast.Ast {
    const l = try parse_loop(ctx, alloc);

    return switch (opt) {
        .expr => Ast.Ast{ .expr = .{ .loop = l } },
        .stmt => Ast.Ast{ .stmt = .{ .loop = l } },
    };
}

pub fn parse_function(ctx: *Context, alloc: std.mem.Allocator) !Ast.Ast {
    _ = ctx;
    _ = alloc;
    undefined;
}

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

const stmt_block_empty = "{};";

const stmt_if_empty = "if true {} else {}";
const stmt_if_else_if_empty = "if true {} else if false {}";
const stmt_if_stmts = "if true {const x = 4;} else {const x = 3;}";

// const assign_if = "const x = if true {3} else {4};";
const assign_if_block = "const x = if true {3} else {4};";
const assign_if_else_if_block_else = "const x = if true {const y = 4;3} else if false {4} else {9};";

const reassign_typed = "const x: num = 3; x = 4;";
const block_typed = "const x: void = {const y = 3;};";

const stmt_block_empty2 = "{}";
const stmt_block_nested = "{{};}";

const stmt_loop_empty = "loop {}";
const stmt_loop_break_empty = "loop {break;}";
const assign_loop_empty = "const x = loop {};";
const assign_loop_break = "const x = loop {break 3;};";
const assign_loop_nested = "const x = loop { loop {break 3;}};";
const assign_loop_nested_break = "const x = loop { loop {break 3;} break 4;};";

const multi = "const x = 3; var y = \"abcd\"; if true {y=\"efgh\";} loop {y=\"ijkl\"; break;}";

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

test "parse stmt_block_empty" {
    try parse_test_generic_str(
        stmt_block_empty,
        "stmt_block_empty",
    );
}

test "parse stmt_if_empty" {
    try parse_test_generic_str(
        stmt_if_empty,
        "stmt_if_empty",
    );
}

test "parse stmt_if_else_if_empty" {
    try parse_test_generic_str(
        stmt_if_else_if_empty,
        "stmt_if_else_if_empty",
    );
}

test "parse stmt_if_stmts" {
    try parse_test_generic_str(
        stmt_if_stmts,
        "stmt_if_stmts",
    );
}

// test "parse assign_if" {
//     try parse_test_generic_str(
//         assign_if,
//         "assign_if",
//     );
// }

test "parse assign_if_block" {
    try parse_test_generic_str(
        assign_if_block,
        "assign_if_block",
    );
}

test "parse assign_if_else_if_block_else" {
    try parse_test_generic_str(
        assign_if_else_if_block_else,
        "assign_if_else_if_block_else",
    );
}

test "parse reassign_typed" {
    try parse_test_generic_str(
        reassign_typed,
        "reassign_typed",
    );
}

test "parse block_typed" {
    try parse_test_generic_str(
        block_typed,
        "block_typed",
    );
}

test "parse stmt_block_empty2" {
    try parse_test_generic_str(
        stmt_block_empty2,
        "stmt_block_empty2",
    );
}

test "parse stmt_block_nested" {
    try parse_test_generic_str(
        stmt_block_nested,
        "stmt_block_nested",
    );
}

test "parse assign_loop_empty" {
    try parse_test_generic_str(
        assign_loop_empty,
        "assign_loop_empty",
    );
}

test "parse stmt_loop_break_empty" {
    try parse_test_generic_str(
        stmt_loop_break_empty,
        "stmt_loop_break_empty",
    );
}

test "parse stmt_loop_empty" {
    try parse_test_generic_str(
        stmt_loop_empty,
        "stmt_loop_empty",
    );
}

test "parse assign_loop_break" {
    try parse_test_generic_str(
        assign_loop_break,
        "assign_loop_break",
    );
}

test "parse assign_loop_nested_break" {
    try parse_test_generic_str(
        assign_loop_nested_break,
        "assign_loop_nested_break",
    );
}

test "parse assign_loop_nested" {
    try parse_test_generic_str(
        assign_loop_nested,
        "assign_loop_nested",
    );
}

//
test "parse multi" {
    try parse_test_generic_str(
        multi,
        "multi",
    );
}

// inline
fn parse_test_generic_str(test_str: []const u8, test_name: []const u8) !void {
    std.log.info("=> starting parse test: {s}\n", .{test_name});
    std.log.info("=> test str: {s}\n", .{test_str});

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var ctx = Context.new_context(test_str);
    const ast = try parse_stmts(&ctx, alloc);
    // _ = ast;
    try Ast.print_ast(ast);

    std.log.info("=> finished parse test: {s}\n", .{test_name});
}
