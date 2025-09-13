const Self = @This();
const std = @import("std");
const token = @import("token.zig");

source: []const u8, // this will be changed to file reader ptr
pos: usize = 0,

const contextError = error{
    OutOfBounds,
};

pub fn skipWhitespace(self: *Self) contextError!void {
    // std.debug.panic("FAIL", .{});
    // while (std.mem.containsAtLeast(u8, token.whitespace, 1, (try self.peekByte())[0..1])) {
    while (self.pos < self.source.len and std.mem.containsAtLeast(
        u8,
        token.whitespace,
        1,
        self.source[self.pos .. self.pos + 1],
    )) {
        self.pos += 1;
    }
}

// ptr to u8 won't be dealloced since source SHOULD live as long as this struct
pub fn peekByte(self: *Self) contextError!*const u8 {
    // don't put this here -> try self.skipWhitespace();

    if (self.pos < self.source.len) {
        return &self.source[self.pos];
    } else {
        // return contextError.OutOfBounds;
        return @ptrCast("");
    }
}

pub fn readByte(self: *Self) !*const u8 {
    const b = peekByte(self) catch |e| return e;
    // const b = peekByte(self) catch return .eof;

    self.pos += 1;
    return b;
}

pub fn peekSectionString(self: *Self) ![]const u8 {
    try self.skipWhitespace();
    const b = try self.peekByte();
    // std.debug.panic("FAIL", .{});

    const exit_on_quote = ex: {
        if (@as([]const u8, @ptrCast(b))[0] == '"') {
            // self.pos += 1;
            break :ex true;
        } else break :ex false;
    };
    var seen_quote = false;

    if (!exit_on_quote and std.mem.containsAtLeast(u8, token.delimiter, 1, b[0..1])) {
        std.log.debug("early ret\n", .{});
        return @ptrCast(b);
    }

    const allow_decimal_point = if (std.ascii.isDigit(b.*)) true else false;
    var seen_decimal_point = false;

    const old_pos = self.pos;

    // while (std.ascii.isAlphanumeric((try self.peekByte()).*)) {
    while (lbl: {
        const c = (try self.peekByte()).*;
        const is_alpha_num = std.ascii.isAlphanumeric(c);
        break :lbl is_alpha_num or
            (allow_decimal_point and (is_alpha_num or c == '.')) or
            (exit_on_quote and (is_alpha_num or c == '"'));
    }) {

        // Capture quoted string -> "hello"
        const peek = (try self.peekByte()).*;

        if (exit_on_quote and !seen_quote and peek == '"') {
            std.log.debug("QUOTE alphanum: {c}\n", .{peek});
            seen_quote = true;
        } else if (exit_on_quote and seen_quote and peek == '"') {
            std.log.debug("AFTER QUOTE alphanum: {c}\n", .{peek});
            self.pos += 1;
            break;
        } else if (allow_decimal_point and !seen_decimal_point and peek == '.') {
            seen_decimal_point = true;
        } else if (allow_decimal_point and seen_decimal_point and peek == '.') {
            return error.IncorrectNumberFormat;
        }

        // std.log.info("alphanum: {c}\n", .{(try self.peekByte()).*});
        // std.log.debug("alphanum: {c}\n", .{peek});

        self.pos += 1;
    }
    defer self.pos = old_pos;
    std.log.debug("section parsed: {s}\n", .{self.source[old_pos..self.pos]});
    return self.source[old_pos..self.pos];
}

pub fn readSectionString(self: *Self) ![]const u8 {
    const str = try self.peekSectionString();

    self.pos += str.len;

    return str;
}

pub fn peekToken(self: *Self) !token.Token {
    return token.matchTok(try self.peekSectionString());
}

pub fn readToken(self: *Self) !token.Token {
    return token.matchTok(try self.readSectionString());
}

pub fn reset(self: *Self, src: anytype) void {
    self.pos = 0;
    self.source = src;
}

pub fn new_context(source: []const u8) Self {
    return .{
        .pos = 0,
        .source = source,
    };
}

const str1 = "";
const str2 = "  abcde";

test "whitespace" {
    var ctx: Self = Self{ .source = str2 };
    try ctx.skipWhitespace();

    try std.testing.expectEqual(2, ctx.pos);

    //
    ctx = Self{ .source = str2 };
    try ctx.skipWhitespace();

    try std.testing.expectEqual(2, ctx.pos);

    std.log.info("whtespace test completed succcesfully\n", .{});
}
