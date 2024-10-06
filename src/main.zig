const std = @import("std");
const c = @import("common.zig");
const compiler = @import("compiler.zig");
const vm = @import("vm.zig");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const args = try std.process.argsAlloc(allocator);

    if (args.len == 1) {
        try repl(allocator);
    } else if (args.len == 2) {
        try runFile(args[1], allocator);
    } else {
        // TODO: Print usage or something
    }
}

fn repl(allocator: std.mem.Allocator) !void {
    const stdout = std.io.getStdOut().writer();

    const in = std.io.getStdIn().reader();
    var br = std.io.bufferedReader(in);
    const stdin = br.reader();

    while (true) {
        try stdout.print(" > ", .{});
        var buf: [1024]u8 = undefined;
        const line_opt = try stdin.readUntilDelimiterOrEof(&buf, '\n');

        if (line_opt) |line| {
            _ = vm.interpret(line, allocator);
        } else {
            break;
        }
    }
}

fn runFile(filename: []u8, allocator: std.mem.Allocator) !void {
    var file = try std.fs.cwd().openFile(filename, .{});
    defer file.close();

    const size = (try file.stat()).size;
    var br = std.io.bufferedReader(file.reader());
    const buffer = try allocator.alloc(u8, size);
    const read = try br.read(buffer);

    if (read < size) {
        // ??
    }

    _ = vm.interpret(buffer, allocator);
}
