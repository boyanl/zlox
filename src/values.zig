const std = @import("std");
const objects = @import("object.zig");

pub const Number = f64;
pub const ValueType = enum { number, boolean, obj, nil };
pub const Value = union(ValueType) { number: Number, boolean: bool, obj: *objects.Obj, nil };
pub const ValueArray = std.ArrayList(Value);

pub fn is_number(v: Value) bool {
    return @as(ValueType, v) == .number;
}

pub fn is_bool(v: Value) bool {
    return @as(ValueType, v) == .boolean;
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

pub fn as_obj(v: Value) *objects.Obj {
    return switch (v) {
        .obj => v.obj,
        else => undefined,
    };
}

pub fn is_object(v: Value) bool {
    return @as(ValueType, v) == .obj;
}

pub fn is_string(v: Value) bool {
    return is_object(v) and v.obj.is_string();
}

pub fn is_nil(v: Value) bool {
    return @as(ValueType, v) == .nil;
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
        .obj => v1.obj.as_string() == v2.obj.as_string(),
    };
}

fn print_string(s: objects.Obj.String) void {
    std.debug.print("{s}", .{s.str});
}

pub fn print_value(v: Value) void {
    switch (v) {
        .number => std.debug.print("{d}", .{as_number(v)}),
        .boolean => std.debug.print("{}", .{as_bool(v)}),
        .nil => std.debug.print("nil", .{}),
        .obj => |item| {
            print_string(item.as_string().*);
        },
    }
}
