const std = @import("std");
const Tok = @import("token");

const MAX_FNS = 512;
const MAX_VARS = 512;

// fns_in_scope: [MAX_FNS]*anyopaque, // Determine the type later - fns are a var
vars_in_scope: [MAX_VARS]*anyopaque, // Determine the type later

const State = union(enum) { variable };

// pub fn state(reader: *std.io.AnyReader) !void {
//     _ = reader;

//     const tok = readToken(reader);

//     state: switch () {}
// }
