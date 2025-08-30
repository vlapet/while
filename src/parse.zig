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
    _ = alloc;
    const tok = try ctx.readToken();

    return switch (tok) {
        .@"if" => unreachable,
        // .literal => |l| Ast.Ast{ .expr = .{ .val_lit = l } },
        .literal => error.UnexpectedLiteral,
        .quoted_literal => |q| Ast.Ast{ .expr = .{ .val_lit = q } },
        .number => |n| Ast.Ast{ .expr = .{ .val_num = n } },
        .true => Ast.Ast{ .expr = .{ .val_bool = true } },
        .false => Ast.Ast{ .expr = .{ .val_bool = false } },
        // .lbrace => unreachable,
        .function => unreachable,
        .@"return" => unreachable,

        else => unreachable,
    };
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

const r1 = "x : 3;"; // no - syntax error
const r2 = "x = 3;"; // no - assigned variable

test "parse" {
    var ctx = Context{
        .pos = 0,
        // .source = v1,
        .source = v1[0..],
    };

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    _ = try parse_stmts(&ctx, alloc);

    ctx.source = v2[0..];
    ctx.pos = 0;

    const ast = try parse_stmts(&ctx, alloc);
    // const ast_check2 = Ast.Ast{
    //     .assign = .{
    //         .@"var" = .{
    //             .ident = "x",
    //             .is_const = true,
    //         },
    //         .expr = .{
    //             .val_num = 3,
    //         },
    //     },
    // };

    // Ast.check_ast(
    //     ast,
    // );

    try Ast.print_ast(ast);
    // try Ast.print_ast(ast_check2);
    // try Ast.check_ast(ast_check2, ast);

    ctx.reset(v3[0..]);

    const ast2 = try parse_stmts(&ctx, alloc);
    try Ast.print_ast(ast2);
    std.debug.print("Parse tests completed successfully!\n", .{});
}

// pub fn parse(ctx: *Context, alloc: std.mem.Allocator) !Ast.Ast {
//     // _ = alloc;
//     // This language is simple so first char must be an alphanum

//     const t = try ctx.readToken();
//     // std.debug.panic("f", .{});
//     std.log.err("Parsing: {s}", .{@tagName(t)});

//     return st: switch (t) {
//         .quoted_literal => |l| {
//             break :st Ast.Ast{ .exprs = .{
//                 .expr = .{ .val_lit = l },
//                 .exprs = null,
//             } };
//         },
//         .literal => |l| // ZIG BUG: Need to remove discarded capture to compile in a labled switch
//         {
//             // const tok = try ctx.readToken();
//             // try check_tok(tok, .assign);
//             // continue :st tok;
//             break :st Ast.Ast{ .exprs = .{
//                 .expr = .{ .val_num = try std.fmt.parseFloat(f64, l) },
//                 .exprs = null,
//             } };
//         },
//         inline .@"const", Token.@"var" => |s, tag| {
//             const is_const = if (tag == Token.@"const")
//                 true
//             else
//                 false;
//             _ = s;

//             const var_lit = try ctx.readToken();
//             try check_tok(var_lit, .{ .literal = "..." });

//             const assign = try ctx.readToken();
//             try check_tok(assign, .assign);

//             // const expr = ctx.parse();
//             const expr = try parse(ctx, alloc);
//             const expr_res = switch (expr) {
//                 .expr => |e| e,
//                 else => |r| std.debug.panic("Unexpected result: {}", .{std.meta.activeTag(r)}),
//             };

//             break :st Ast.Ast{
//                 .assign = .{
//                     .@"var" = Ast.Var{
//                         .ident = var_lit.literal,
//                         .is_const = is_const,
//                     },
//                     .expr = expr_res,
//                 },
//             };
//             // continue :st tok;
//         },
//         .@"if" => {
//             @panic("unimpl");
//         },
//         .lparen => continue :st .assign,
//         .semicolon => return .semicolon,
//         .eof => return .eof,

//         // else => continue :st token.Token.lparen,
//         else => @panic("efgh"),
//     };
// }
