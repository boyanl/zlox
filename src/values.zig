const std = @import("std");
const objects = @import("object.zig");

pub const Number = f64;
const ValueType = enum { number, boolean, nil };
pub const Value = union(ValueType) { number: Number, boolean: bool, nil };
pub const ValueArray = std.ArrayList(Value);

pub fn is_number(v: Value) bool {
    return switch (v) {
        .number => true,
        else => false,
    };
}

pub fn is_bool(v: Value) bool {
    return switch (v) {
        .boolean => true,
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
        .boolean => v.boolean,
        else => undefined,
    };
}

fn value_type(v: Value) ValueType {
    return switch (v) {
        .boolean => .boolean,
        .number => .number,
        .nil => .nil,
    };
}

pub fn values_equal(v1: Value, v2: Value) bool {
    if (@as(ValueType, v1) != @as(ValueType, v2)) return false;

    return switch (v1) {
        .boolean => as_bool(v1) == as_bool(v2),
        .nil => true,
        .number => as_number(v1) == as_number(v2),
    };
}

pub fn print_value(v: Value) void {
    switch (v) {
        .number => std.debug.print("{d}", .{as_number(v)}),
        .boolean => std.debug.print("{}", .{as_bool(v)}),
        .nil => std.debug.print("nil", .{}),
    }
}
