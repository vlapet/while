const std = @import("std");

const token = @import("token.zig");
const parse = @import("parse.zig");
const context = @import("context.zig");
const compiler = @import("compiler.zig");

pub const std_options: std.Options = .{
    // Set the log level to info
    .log_level = .info,

    // Define logFn to override the std implementation
    // .logFn = myLogFn,

};

pub const log_level: std.log.Level = .info;

// Example:
// const f = fn(x: i32, y: i32){
//   x + y
// }
//
// const y = f(2,3);
//

pub fn main() !void {
    // const args = std.process.args();

}

test "tests" {
    std.testing.log_level = .debug;
    // std.testing.log_level = .info;

    // _ = token;
    _ = parse;
    _ = compiler;
    // _ = context;
}
