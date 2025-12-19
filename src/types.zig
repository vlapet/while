const std = @import("std");
const Ast = @import("ast.zig");

const POINTER_SIZE = 8;

pub const FnType = struct {
    params: []SysType,
    ret_type: []SysType,
};

// pub const function = struct {
//     head: FnHead,
//     body: Block,
// };

// pub const Type = union(enum) {};

pub const PointerVariant = union(enum) {
    Single: struct { addr: u8 }, // ptr to single item
    ManyUnknown: struct { addr: u8 }, // ptr to many items - amount of items is UNKNOWN
    ManyKnown: struct { len: u8, addr: u8 }, // ptr to many items - amount of items is KNOWN - basically ptr to slice
    Slice: struct { len: u8, addr: u8 }, // slice of array
};

pub const PointerType = struct {
    type: *SysType,
    ptr_variant: PointerVariant,
};

pub const SysType = union(enum) {
    Void: void,
    // Int: u8, // tbd
    Float: u8, // size tbd
    Char: void, // this is u8 but we will differentiate
    // String: void, // tbd: do we want to support this explicitly?
    Null: void,
    Function: FnType,
    Pointer: PointerType,
    // Struct: StructType,
    // Union: UnionType,
    // Enum: EnumType,
};

pub fn get_alignment_bytes(T: *const SysType) u8 {
    switch (T.*) {
        .Void => 0,
        .Float => |f| f / 8, // size stored in bits - so divide by 8 to get bytes
        .Char => 1,
        .Null => 0,
        .Function => std.debug.panic("NOT IMPLEMENTED - ALIGN OF FUNCTION", {}),
    }
}

pub const StructType = .{};

pub fn eval_sys_type_expr(expr: ?*const Ast.Expr) !SysType {
    if (expr) |n_expr| {
        return switch (n_expr.*) {
            .basic_var => |b| switch (b) {
                .val_lit => std.debug.panic("not implemented yet", .{}),
                .val_num => std.debug.panic("not implemented yet", .{}),
                .val_bool => std.debug.panic("not implemented yet", .{}),
                .val_char => std.debug.panic("not implemented yet", .{}),
                .val_void => std.debug.panic("not implemented yet", .{}),
            },
            .@"if" => |i| {
                const if_expr = try eval_sys_type_expr(i.if_expr);
                const else_expr = try eval_sys_type_expr(i.else_expr);
                _ = if_expr;
                _ = else_expr;
                std.debug.panic("not implemented yet", .{});
            },
            .block => |b| try eval_sys_type_expr(b.expr),
            .loop => |l| b: {
                const a = Ast.Expr{ .block = l.block };
                break :b try eval_sys_type_expr(&a);
            },
            // inline else => |e| try eval_sys_type_expr(e),
        };
    } else {
        return SysType.Null;
    }
}
