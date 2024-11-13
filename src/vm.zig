const std = @import("std");
const common = @import("common.zig");
const vals = @import("values.zig");
const obj = @import("object.zig");
const table = @import("table.zig");
const Compiler = @import("compiler.zig").Compiler;
const assert = std.debug.assert;

pub const InterpretResult = enum { OK, COMPILE_ERROR, RUNTIME_ERROR };
const STACK_MAX = 1024;

pub const VM = struct {
    chunk: ?*common.Chunk = null,
    ip: usize = 0,
    stack: [STACK_MAX]vals.Value = [_]vals.Value{undefined} ** STACK_MAX,
    stack_top: usize = 0, // TODO: Tried to use a pointer here but it gets copied when init() returns and points to an invalid address ..
    allocator: std.mem.Allocator,
    objects: ?*obj.Obj = null,
    strings: table.Table,

    pub fn init(allocator: std.mem.Allocator) VM {
        return VM{ .allocator = allocator, .strings = table.Table.init(allocator) };
    }
    pub fn deinit(self: *VM) void {
        self.strings.deinit();

        var current = self.objects;
        while (current != null) {
            const next = current.?.next;
            current.?.deinit(self);
            current = next;
        }
    }

    pub fn interpret(self: *VM, source: []u8, allocator: std.mem.Allocator) InterpretResult {
        var chunk = common.Chunk.init(allocator);
        defer chunk.deinit();

        var compiler = Compiler.init(self, self.allocator);

        if (!compiler.compile(source, &chunk)) return .COMPILE_ERROR;

        self.chunk = &chunk;
        self.ip = 0;
        return self.run();
    }

    fn read_byte(self: *VM) u8 {
        const old = self.chunk.?.code.items[self.ip];
        self.ip += 1;
        return old;
    }

    fn read_constant(self: *VM) vals.Value {
        return self.chunk.?.constants.items[self.read_byte()];
    }

    fn reset_stack(self: *VM) void {
        self.stack_top = 0;
    }

    fn push(self: *VM, val: vals.Value) void {
        self.stack[self.stack_top] = val;
        self.stack_top += 1;
    }

    fn pop(self: *VM) vals.Value {
        assert(self.stack_top > 0);
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    fn peek(self: *VM, depth: usize) vals.Value {
        assert(self.stack_top - depth - 1 >= 0);
        return self.stack[self.stack_top - depth - 1];
    }

    fn runtime_error(self: *VM, comptime fmt: []const u8, args: anytype) void {
        std.debug.print(fmt, args);
        std.debug.print("\n", .{});

        const instr = self.ip - 1;
        const line = self.chunk.?.get_line(instr);
        std.debug.print("[line {d}] in script", .{line});
        self.reset_stack();
    }

    fn binary_numeric_op(self: *VM, instr: common.InstructionType) InterpretResult {
        if (!vals.is_number(self.peek(0)) or !vals.is_number(self.peek(1))) {
            self.runtime_error("Operands must be numbers", .{});
            return .RUNTIME_ERROR;
        }

        const b = self.pop().number;
        const a = self.pop().number;
        switch (instr) {
            .OP_ADD => self.push(vals.Value{ .number = a + b }),
            .OP_SUBTRACT => self.push(vals.Value{ .number = a - b }),
            .OP_MULTIPLY => self.push(vals.Value{ .number = a * b }),
            .OP_DIVIDE => self.push(vals.Value{ .number = a / b }),
            .OP_GREATER => self.push(.{ .boolean = a > b }),
            .OP_LESS => self.push(.{ .boolean = a < b }),
            else => undefined,
        }

        return .OK;
    }

    fn concatenate(self: *VM) void {
        const s2 = self.pop().obj.as_string();
        const s1 = self.pop().obj.as_string();
        var s = self.allocator.alloc(u8, s1.str.len + s2.str.len) catch return;

        std.mem.copyForwards(u8, s, s1.str);
        std.mem.copyForwards(u8, s[s1.str.len..], s2.str);

        const res = obj.take_string(s, self) catch return;
        self.push(.{ .obj = res.as_obj() });
    }

    pub fn is_falsey(v: vals.Value) bool {
        return switch (v) {
            .nil => true,
            .boolean => v.boolean == false,
            else => false,
        };
    }

    pub fn run(self: *VM) InterpretResult {
        while (true) {
            if (common.DEBUG) {
                _ = common.disassemble_instruction(self.chunk.?.*, self.ip);
            }

            const instruction: common.InstructionType = @enumFromInt(self.read_byte());
            switch (instruction) {
                .OP_RETURN => {
                    vals.print_value(self.pop());
                    std.debug.print("\n", .{});
                    return .OK;
                },
                .OP_CONSTANT => {
                    self.push(self.read_constant());
                },
                .OP_NEGATE => {
                    if (!vals.is_number(self.peek(0))) {
                        self.runtime_error("Operand must be a number", .{});
                        return .RUNTIME_ERROR;
                    }
                    self.push(vals.Value{ .number = -vals.as_number(self.pop()) });
                },
                .OP_ADD => {
                    if (vals.is_string(self.peek(0)) and vals.is_string(self.peek(1))) {
                        self.concatenate();
                    } else if (vals.is_number(self.peek(0)) and vals.is_number(self.peek(1))) {
                        const b = self.pop().number;
                        const a = self.pop().number;
                        self.push(vals.Value{ .number = a + b });
                    } else {
                        self.runtime_error("Operands must be either numbers or strings", .{});
                        return .RUNTIME_ERROR;
                    }
                },
                .OP_SUBTRACT, .OP_MULTIPLY, .OP_DIVIDE, .OP_GREATER, .OP_LESS => {
                    if (self.binary_numeric_op(instruction) == .RUNTIME_ERROR) {
                        return .RUNTIME_ERROR;
                    }
                },
                .OP_TRUE => self.push(.{ .boolean = true }),
                .OP_FALSE => self.push(.{ .boolean = false }),
                .OP_NIL => self.push(.nil),
                .OP_NOT => self.push(.{ .boolean = is_falsey(self.pop()) }),

                .OP_EQUAL => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(.{ .boolean = vals.values_equal(a, b) });
                },
            }
        }
    }
};
