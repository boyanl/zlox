const std = @import("std");
const common = @import("common.zig");
const vals = @import("values.zig");
const compiler = @import("compiler.zig");
const assert = std.debug.assert;

pub const InterpretResult = enum { OK, COMPILE_ERROR, RUNTIME_ERROR };
const STACK_MAX = 1024;

pub const VM = struct {
    chunk: ?*common.Chunk = null,
    ip: usize = 0,
    stack: [STACK_MAX]vals.Value = [_]vals.Value{undefined} ** STACK_MAX,
    stack_top: usize = 0, // TODO: Tried to use a pointer here but it gets copied when init() returns and points to an invalid address ..

    pub fn init() VM {
        return VM{};
    }
    pub fn deinit() void {}
};

var vm = VM.init();

pub fn interpret(source: []u8, allocator: std.mem.Allocator) InterpretResult {
    var chunk = common.Chunk.init(allocator);
    defer chunk.deinit();

    if (!compiler.compile(source, &chunk)) return .COMPILE_ERROR;

    vm.chunk = &chunk;
    vm.ip = 0;
    return run();
}

fn read_byte() u8 {
    const old = vm.chunk.?.code.items[vm.ip];
    vm.ip += 1;
    return old;
}

fn read_constant() vals.Value {
    return vm.chunk.?.constants.items[read_byte()];
}

fn reset_stack() void {
    vm.stack_top = 0;
}

fn push(val: vals.Value) void {
    vm.stack[vm.stack_top] = val;
    vm.stack_top += 1;
}

fn pop() vals.Value {
    assert(vm.stack_top > 0);
    vm.stack_top -= 1;
    return vm.stack[vm.stack_top];
}

fn peek(depth: usize) vals.Value {
    assert(vm.stack_top - depth - 1 >= 0);
    return vm.stack[vm.stack_top - depth - 1];
}

fn runtime_error(comptime fmt: []const u8, args: anytype) void {
    std.debug.print(fmt, args);
    std.debug.print("\n", .{});

    const instr = vm.ip - 1;
    const line = vm.chunk.?.get_line(instr);
    std.debug.print("[line {d}] in script", .{line});
    reset_stack();
}

fn binary_op(instr: common.InstructionType) InterpretResult {
    if (!vals.is_number(peek(0)) or !vals.is_number(peek(1))) {
        runtime_error("Operands must be numbers", .{});
        return .RUNTIME_ERROR;
    }

    const b = pop().number;
    const a = pop().number;
    switch (instr) {
        .OP_ADD => push(vals.Value{ .number = a + b }),
        .OP_SUBTRACT => push(vals.Value{ .number = a - b }),
        .OP_MULTIPLY => push(vals.Value{ .number = a * b }),
        .OP_DIVIDE => push(vals.Value{ .number = a / b }),
        else => undefined,
    }

    return .OK;
}

pub fn run() InterpretResult {
    while (true) {
        if (common.DEBUG) {
            _ = common.disassemble_instruction(vm.chunk.?.*, vm.ip);
        }

        const instruction: common.InstructionType = @enumFromInt(read_byte());
        switch (instruction) {
            .OP_RETURN => {
                vals.printValue(pop());
                std.debug.print("\n", .{});
                return .OK;
            },
            .OP_CONSTANT => {
                push(read_constant());
            },
            .OP_NEGATE => {
                if (!vals.is_number(peek(0))) {
                    runtime_error("Operand must be a number", .{});
                    return .RUNTIME_ERROR;
                }
                push(vals.Value{ .number = -vals.as_number(pop()) });
            },
            .OP_ADD, .OP_SUBTRACT, .OP_MULTIPLY, .OP_DIVIDE => {
                if (binary_op(instruction) == .RUNTIME_ERROR) {
                    return .RUNTIME_ERROR;
                }
            },
        }
    }
}
