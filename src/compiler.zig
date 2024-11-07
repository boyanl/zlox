const std = @import("std");
const common = @import("common.zig");
const s = @import("scanner.zig");
const vals = @import("values.zig");

// TODO: Can we do better than leaving those undefined and initializing them later?
const Parser = struct {
    previous: s.Token = undefined,
    current: s.Token = undefined,
    scanner: s.Scanner = undefined,
    had_error: bool = false,
    panic_mode: bool = false,
};

var parser = Parser{};
var compiling_chunk: ?*common.Chunk = null;

pub fn compile(source: []u8, chunk: *common.Chunk) bool {
    parser.scanner = s.Scanner.init(source);

    parser.had_error = false;
    parser.panic_mode = false;

    compiling_chunk = chunk;

    advance();
    expression();
    consume(.EOF, "Expected end after expression.");

    end_compiler();
    return !parser.had_error; // TODO: Use errors for this instead of returning a boolean?
}

fn advance() void {
    parser.previous = parser.current;

    while (true) {
        parser.current = parser.scanner.next_token();
        if (parser.current.type != .ERROR) break;

        error_at_current(parser.current.lexeme);
    }
}

const Precedence = enum(u8) {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY,
};

const ParseFn = *const fn () void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

fn grouping() void {
    expression();
    consume(.RIGHT_PAREN, "Expect ')' after expression");
}

fn expression() void {
    parse_precedence(.ASSIGNMENT);
}

fn number() void {
    const val = std.fmt.parseFloat(vals.Number, parser.previous.lexeme) catch {
        error_at_prev("Can't parse number");
        return;
    };
    emit_constant(vals.Value{ .number = val });
}

fn unary() void {
    const opType = parser.previous.type;
    parse_precedence(.UNARY);

    switch (opType) {
        .MINUS => emit_instruction(.OP_NEGATE),
        .BANG => emit_instruction(.OP_NOT),
        else => unreachable,
    }
}

fn binary() void {
    const opType = parser.previous.type;
    const rule = get_rule(opType);

    parse_precedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

    switch (opType) {
        .PLUS => emit_instruction(.OP_ADD),
        .MINUS => emit_instruction(.OP_SUBTRACT),
        .STAR => emit_instruction(.OP_MULTIPLY),
        .SLASH => emit_instruction(.OP_DIVIDE),
        .EQUAL_EQUAL => emit_instruction(.OP_EQUAL),
        .BANG_EQUAL => emit_instructions(.OP_EQUAL, .OP_NOT),
        .GREATER => emit_instruction(.OP_GREATER),
        .GREATER_EQUAL => emit_instructions(.OP_LESS, .OP_NOT),
        .LESS => emit_instruction(.OP_LESS),
        .LESS_EQUAL => emit_instructions(.OP_GREATER, .OP_NOT),

        else => unreachable,
    }
}

fn literal() void {
    switch (parser.previous.type) {
        .TRUE => emit_instruction(.OP_TRUE),
        .FALSE => emit_instruction(.OP_FALSE),
        .NIL => emit_instruction(.OP_NIL),
        else => unreachable,
    }
}

const rules = init: {
    const count = @typeInfo(s.Token.Type).Enum.fields.len;

    var arr = [_]ParseRule{.{ .prefix = null, .infix = null, .precedence = .NONE }} ** count;
    const t = s.Token.Type;
    arr[@intFromEnum(t.LEFT_PAREN)] = .{ .prefix = grouping, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.MINUS)] = .{ .prefix = unary, .infix = binary, .precedence = .TERM };
    arr[@intFromEnum(t.PLUS)] = .{ .prefix = null, .infix = binary, .precedence = .TERM };
    arr[@intFromEnum(t.SLASH)] = .{ .prefix = null, .infix = binary, .precedence = .FACTOR };
    arr[@intFromEnum(t.STAR)] = .{ .prefix = null, .infix = binary, .precedence = .FACTOR };
    arr[@intFromEnum(t.NUMBER)] = .{ .prefix = number, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.TRUE)] = .{ .prefix = literal, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.FALSE)] = .{ .prefix = literal, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.NIL)] = .{ .prefix = literal, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.BANG)] = .{ .prefix = unary, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.EQUAL_EQUAL)] = .{ .prefix = null, .infix = binary, .precedence = .EQUALITY };
    arr[@intFromEnum(t.BANG_EQUAL)] = .{ .prefix = null, .infix = binary, .precedence = .EQUALITY };
    arr[@intFromEnum(t.GREATER)] = .{ .prefix = null, .infix = binary, .precedence = .COMPARISON };
    arr[@intFromEnum(t.GREATER_EQUAL)] = .{ .prefix = null, .infix = binary, .precedence = .COMPARISON };
    arr[@intFromEnum(t.LESS)] = .{ .prefix = null, .infix = binary, .precedence = .COMPARISON };
    arr[@intFromEnum(t.LESS_EQUAL)] = .{ .prefix = null, .infix = binary, .precedence = .COMPARISON };

    break :init arr;
};

fn get_rule(t: s.Token.Type) ParseRule {
    return rules[@intFromEnum(t)];
}

fn parse_precedence(p: Precedence) void {
    advance();
    const prefix_rule = get_rule(parser.previous.type).prefix;

    if (prefix_rule) |f| {
        f();
    } else {
        error_at_prev("Expected expression");
        return;
    }

    while (@intFromEnum(p) <= @intFromEnum(get_rule(parser.current.type).precedence)) {
        advance();
        const infix_rule = get_rule(parser.previous.type).infix;
        if (infix_rule) |f| {
            f();
        } else {
            error_at_prev("No infix rule lul");
            return;
        }
    }
}

fn error_at_current(msg: []const u8) void {
    error_at(&parser.current, msg) catch {
        std.debug.print("Can't output error msg\n", .{});
    };
}

fn error_at_prev(msg: []const u8) void {
    error_at(&parser.previous, msg) catch {
        std.debug.print("Can't output error msg\n", .{});
    };
}

fn error_at(token: *s.Token, msg: []const u8) !void {
    if (parser.panic_mode) return;

    parser.panic_mode = true;
    const stderr = std.io.getStdErr().writer();

    try stderr.print("[line {d}] Error", .{token.line});

    switch (token.type) {
        .EOF => try stderr.print(" at end", .{}),
        .ERROR => {},
        else => try stderr.print(" at '{s}'", .{token.lexeme}),
    }

    try stderr.print(": {s}\n", .{msg});

    parser.had_error = true;
}

fn consume(token_type: s.Token.Type, msg: []const u8) void {
    if (parser.current.type == token_type) {
        advance();
        return;
    }

    error_at_current(msg);
}

fn current_chunk() *common.Chunk {
    return compiling_chunk.?;
}

fn emit_byte(b: u8) void {
    current_chunk().write(b, @intCast(parser.previous.line)) catch return;
}

fn emit_bytes(b1: u8, b2: u8) void {
    emit_byte(b1);
    emit_byte(b2);
}

fn emit_instruction(t: common.InstructionType) void {
    emit_byte(@intFromEnum(t));
}

fn emit_instructions(t1: common.InstructionType, t2: common.InstructionType) void {
    emit_instruction(t1);
    emit_instruction(t2);
}

fn emit_constant(v: vals.Value) void {
    emit_bytes(@intFromEnum(common.InstructionType.OP_CONSTANT), make_constant(v));
}

fn make_constant(v: vals.Value) u8 {
    const idx = current_chunk().add_constant(v);
    if (idx >= std.math.maxInt(u8)) {
        error_at_prev("Too many constants in one chunk");
        return 0;
    }

    return @intCast(idx);
}

fn end_compiler() void {
    emit_instruction(.OP_RETURN);
}
