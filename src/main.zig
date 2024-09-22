const std = @import("std");
const c = @import("common.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var chunk = c.Chunk.init(allocator);
    defer chunk.deinit();

    try chunk.write(@intFromEnum(c.InstructionType.OP_RETURN), 123);

    const const_idx = try chunk.addConstant(1.2);
    try chunk.write(@intFromEnum(c.InstructionType.OP_CONSTANT), 123);
    try chunk.write(@intCast(const_idx), 123);
    try chunk.write(@intFromEnum(c.InstructionType.OP_RETURN), 124);

    chunk.disassemble("test chunk");
    // // stdout is for the actual output of your application, for example if you
    // // are implementing gzip, then only the compressed bytes should be sent to
    // // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();

    // try stdout.print("Run `zig build test` to run the tests.\n", .{});

    // try bw.flush(); // don't forget to flush!
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
