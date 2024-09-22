const std = @import("std");

pub const Value = f64;
pub const ValueArray = std.ArrayList(Value);

pub fn printValue(v: Value) void {
    std.debug.print("{d}", .{v});
}
