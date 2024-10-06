const std = @import("std");

pub const Number = f64;
pub const Value = union(enum) { number: Number, boolean: bool, nil };
pub const ValueArray = std.ArrayList(Value);

pub fn is_number(v: Value) bool {
    return switch (v) {
        .number => true,
        else => false,
    };
}

pub fn is_bool(v: Value) bool {
    return switch (v) {
        .bool => true,
        else => false,
    };
}

pub fn as_number(v: Value) f64 {
    return switch (v) {
        .number => v.number,
        else => undefined,
    };
}

pub fn as_bool(v: Value) bool {
    return switch (v) {
        .bool => v.boolean,
        else => undefined,
    };
}

pub fn printValue(v: Value) void {
    // TODO
    std.debug.print("{d}", .{as_number(v)});
}
