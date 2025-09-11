const std = @import("std");
const context = @import("context.zig");

pub const Token = union(enum) {
    literal: []const u8,
    number: f64,
    quoted_literal: []const u8, // same thing but surrounded in quotes
    @"while",
    @"if",
    @"else",
    lparen,
    rparen,
    lbrace,
    rbrace,
    lbracket,
    rbracket,
    @"const",
    @"var",
    assign, // =
    semicolon,
    colon,
    eof,
    function,
    @"return",
    true,
    false,
};

const TokenMap = std.StaticStringMap(Token).initComptime(
    .{
        .{ "{", .lbrace },
        .{ "}", .rbrace },
        .{ "(", .lparen },
        .{ ")", .rparen },
        .{ "[", .lbracket },
        .{ "]", .rbracket },
        .{ "=", .assign },
        .{ "while", .@"while" },
        .{ "if", .@"if" },
        .{ "else", .@"else" },
        .{ "const", .@"const" },
        .{ "var", .@"var" },
        // .{ "assign", .assign },
        // .{ "assign", .assign },
        // .{ "semicolon", .semicolon },
        // .{ "colon", .colon },
        .{ ";", .semicolon },
        .{ ":", .colon },
        .{ "", .eof },
        .{ "", .function },
        .{ "true", .true },
        .{ "false", .false },
    },
);

pub fn matchTok(str: []const u8) !Token {
    std.log.debug("Matching str: {s}\n", .{str});
    // std.debug.panic("temp: {s}\n", .{str});
    if (TokenMap.get(str)) |t| {
        // @branchHint(.likely);
        return t;
    } else if (str[0] == '"' and str[str.len - 1] == '"') {
        return .{ .quoted_literal = str[1 .. str.len - 1] }; // Not incl uppedbound to range - I think
    } else if (str.len == 1 and !std.ascii.isAlphanumeric(str[0])) {
        // std.debug.print("Token not found: {s}\n", .{str[0..]});
        std.log.err("Token not found: {s}\n", .{str});
        return error.TokenNotFound;
    }

    // if (lbl: {
    //     for (str) |c| {
    //         if (std.ascii.isAlphabetic(c)) break :lbl false;
    //     }
    //     break :lbl true;
    // }) {
    //     std.debug.print("Parsing float str: {s}\n", .{str});
    //     return .{ .number = try std.fmt.parseFloat(f64, str) };
    // }

    if (std.fmt.parseFloat(f64, str) catch null) |f| {
        return .{ .number = f };
    }

    // TODO: Need to handle other possible error cases
    return .{ .literal = str };
}

pub const delimiter = ",.<>/?;:'@#~[{]}-_=+)(*&^%$£$\"!)}]\\|";
pub const whitespace = " \t\r\n";

test "toks" {
    // try std.testing.expectEqual(.lparen, try matchTok("("));
    // try std.testing.expectEqual(Token{ .literal = "abcd" }, try matchTok("abcd"));
    // try std.testing.expectError(error.TokenNotFound, matchTok("@"));

    // std.debug.print("Token tests completed successfully!\n", .{});
}
