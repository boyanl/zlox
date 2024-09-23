const std = @import("std");
const c = @import("common.zig");
const vm = @import("vm.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var chunk = c.Chunk.init(allocator);
    defer chunk.deinit();

    var const_idx = try chunk.addConstant(1.2);
    try chunk.write(@intFromEnum(c.InstructionType.OP_CONSTANT), 123);
    try chunk.write(@intCast(const_idx), 123);

    const_idx = try chunk.addConstant(3.4);
    try chunk.write(@intFromEnum(c.InstructionType.OP_CONSTANT), 123);
    try chunk.write(@intCast(const_idx), 123);

    try chunk.write(@intFromEnum(c.InstructionType.OP_ADD), 123);

    const_idx = try chunk.addConstant(5.6);
    try chunk.write(@intFromEnum(c.InstructionType.OP_CONSTANT), 123);
    try chunk.write(@intCast(const_idx), 123);

    try chunk.write(@intFromEnum(c.InstructionType.OP_DIVIDE), 123);
    try chunk.write(@intFromEnum(c.InstructionType.OP_NEGATE), 123);
    try chunk.write(@intFromEnum(c.InstructionType.OP_RETURN), 124);

    _ = vm.interpret(&chunk);

    // chunk.disassemble("test chunk");
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
