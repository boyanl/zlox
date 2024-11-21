const std = @import("std");
const vals = @import("values.zig");
const assert = std.debug.assert;

pub const InstructionType = enum(u8) {
    OP_CONSTANT,
    OP_RETURN,
    OP_NEGATE,
    OP_ADD,
    OP_SUBTRACT,
    OP_MULTIPLY,
    OP_DIVIDE,
    OP_TRUE,
    OP_FALSE,
    OP_NIL,
    OP_NOT,
    OP_EQUAL,
    OP_GREATER,
    OP_LESS,
    OP_PRINT,
    OP_POP,
    OP_DEFINE_GLOBAL,
    OP_GET_GLOBAL,
    OP_SET_GLOBAL,
};
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
            offset = disassemble_instruction(self, offset);
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

    pub fn add_constant(self: *Chunk, val: vals.Value) usize {
        self.*.constants.append(val) catch {
            std.debug.print("Snafu - can't append to constants in chunk", .{});
        };
        return self.*.constants.items.len - 1;
    }

    pub fn get_line(self: Chunk, offset: usize) u24 {
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

pub fn disassemble_instruction(chunk: Chunk, offset: usize) usize {
    std.debug.print("{d:0>4} ", .{offset});
    const lineNow = chunk.get_line(offset);
    if (offset > 0 and chunk.get_line(offset - 1) == lineNow) {
        std.debug.print("   | ", .{});
    } else {
        std.debug.print("{d:>4} ", .{lineNow});
    }
    const t: InstructionType = @enumFromInt(chunk.code.items[offset]);
    return switch (t) {
        .OP_CONSTANT => constant_instruction("OP_CONSTANT", chunk, offset),
        .OP_RETURN => simple_instruction("OP_RETURN", offset),
        .OP_NEGATE => simple_instruction("OP_NEGATE", offset),
        .OP_ADD => simple_instruction("OP_ADD", offset),
        .OP_SUBTRACT => simple_instruction("OP_SUBTRACT", offset),
        .OP_MULTIPLY => simple_instruction("OP_MULTIPLY", offset),
        .OP_DIVIDE => simple_instruction("OP_DIVIDE", offset),
        .OP_TRUE => simple_instruction("OP_TRUE", offset),
        .OP_FALSE => simple_instruction("OP_FALSE", offset),
        .OP_NIL => simple_instruction("OP_NIL", offset),
        .OP_NOT => simple_instruction("OP_NOT", offset),
        .OP_EQUAL => simple_instruction("OP_EQUAL", offset),
        .OP_GREATER => simple_instruction("OP_GREATER", offset),
        .OP_LESS => simple_instruction("OP_LESS", offset),
        .OP_PRINT => simple_instruction("OP_PRINT", offset),
        .OP_POP => simple_instruction("OP_POP", offset),
        .OP_DEFINE_GLOBAL => constant_instruction("OP_DEFINE_GLOBAL", chunk, offset),
        .OP_GET_GLOBAL => constant_instruction("OP_GET_GLOBAL", chunk, offset),
        .OP_SET_GLOBAL => constant_instruction("OP_SET_GLOBAL", chunk, offset),
    };
}

fn constant_instruction(name: []const u8, chunk: Chunk, offset: usize) usize {
    const const_idx = chunk.code.items[offset + 1];
    std.debug.print("{s:<16} {d:>4} '", .{ name, const_idx });
    vals.print_value(chunk.constants.items[const_idx]);
    std.debug.print("'\n", .{});

    return offset + 2;
}

fn simple_instruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}
