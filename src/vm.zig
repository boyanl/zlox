const std = @import("std");
const common = @import("common.zig");
const vals = @import("values.zig");
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

pub fn interpret(chunk: *common.Chunk) InterpretResult {
    vm.chunk = chunk;
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
    vm.stack_top = &vm.stack;
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

fn binaryOp(instr: common.InstructionType) void {
    const b = pop();
    const a = pop();
    switch (instr) {
        .OP_ADD => push(a + b),
        .OP_SUBTRACT => push(a - b),
        .OP_MULTIPLY => push(a * b),
        .OP_DIVIDE => push(a / b),
        else => undefined,
    }
}

pub fn run() InterpretResult {
    while (true) {
        if (common.DEBUG) {
            _ = common.disassembleInstruction(vm.chunk.?.*, vm.ip);
        }

        const instruction: common.InstructionType = @enumFromInt(read_byte());
        switch (instruction) {
            .OP_RETURN => {
                vals.printValue(pop());
                return .OK;
            },
            .OP_CONSTANT => {
                push(read_constant());
            },
            .OP_NEGATE => {
                push(-pop());
            },
            .OP_ADD, .OP_SUBTRACT, .OP_MULTIPLY, .OP_DIVIDE => binaryOp(instruction),
        }
    }
}
