const std = @import("std");
const vals = @import("values.zig");
const assert = std.debug.assert;

pub const InstructionType = enum(u8) { OP_CONSTANT, OP_RETURN, OP_NEGATE, OP_ADD, OP_SUBTRACT, OP_MULTIPLY, OP_DIVIDE };
pub const LineNumber = u24;
pub const LineRun = packed struct { cnt: u8, line: LineNumber };
pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: vals.ValueArray,
    lines: std.ArrayList(LineRun),

    pub fn init(allocator: std.mem.Allocator) Chunk {
        return Chunk{ .code = std.ArrayList(u8).init(allocator), .constants = vals.ValueArray.init(allocator), .lines = std.ArrayList(LineRun).init(allocator) };
    }

    pub fn deinit(self: Chunk) void {
        self.code.deinit();
        self.constants.deinit();
        self.lines.deinit();
    }

    pub fn disassemble(self: Chunk, name: []const u8) void {
        std.debug.print("== {s} ==\n", .{name});

        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = disassembleInstruction(self, offset);
        }
    }

    pub fn write(self: *Chunk, byte: u8, line: LineNumber) !void {
        try self.*.code.append(byte);

        if (self.*.lines.items.len > 0) {
            const last = self.*.lines.getLast();
            if (last.line == line and last.cnt < std.math.maxInt(@TypeOf(last.cnt))) {
                self.*.lines.items[self.*.lines.items.len - 1].cnt += 1;
                return;
            }
        }
        try self.*.lines.append(LineRun{ .cnt = 1, .line = line });
    }

    pub fn addConstant(self: *Chunk, val: vals.Value) !usize {
        try self.*.constants.append(val);
        return self.*.constants.items.len - 1;
    }

    pub fn getLine(self: Chunk, offset: usize) u24 {
        assert(offset < self.code.items.len);

        var remaining = offset;
        for (self.lines.items) |lineRun| {
            if (remaining < lineRun.cnt) {
                return lineRun.line;
            }
            remaining -= lineRun.cnt;
        }

        // TODO: Return an error instead?
        assert(false);
        return 0;
    }
};

pub const DEBUG = true;

pub fn disassembleInstruction(chunk: Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    const lineNow = chunk.getLine(offset);
    if (offset > 0 and chunk.getLine(offset - 1) == lineNow) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:>4} ", .{lineNow});
    }
    const t: InstructionType = @enumFromInt(chunk.code.items[offset]);
    return switch (t) {
        .OP_CONSTANT => constantInstruction("OP_CONSTANT", chunk, offset),
        .OP_RETURN => simpleInstruction("OP_RETURN", offset),
        .OP_NEGATE => simpleInstruction("OP_NEGATE", offset),
        .OP_ADD => simpleInstruction("OP_ADD", offset),
        .OP_SUBTRACT => simpleInstruction("OP_SUBTRACT", offset),
        .OP_MULTIPLY => simpleInstruction("OP_MULTIPLYl", offset),
        .OP_DIVIDE => simpleInstruction("OP_DIVIDE", offset),
    };
}

fn constantInstruction(name: []const u8, chunk: Chunk, offset: usize) usize {
    const const_idx = chunk.code.items[offset + 1];
    std.debug.print("{s:<16} {d:>4} '", .{ name, const_idx });
    vals.printValue(chunk.constants.items[const_idx]);
    std.debug.print("'\n", .{});

    return offset + 2;
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
