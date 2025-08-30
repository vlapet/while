/// The interpreter lazily interprets the file
const std = @import("std");
const context = @import("context.zig");
const state = @import("state.zig");

const Self = @This();

pub fn eval_type() !type {} // use zig's underlying type system
