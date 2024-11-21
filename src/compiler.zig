const std = @import("std");
const common = @import("common.zig");
const s = @import("scanner.zig");
const vals = @import("values.zig");
const objs = @import("object.zig");
const VM = @import("vm.zig").VM;

// TODO: Can we do better than leaving those undefined and initializing them later?
const Parser = struct {
    previous: s.Token = undefined,
    current: s.Token = undefined,
    scanner: s.Scanner = undefined,
    had_error: bool = false,
    panic_mode: bool = false,
};

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

const ParseFn = *const fn (p: *Compiler, can_assign: bool) void;

const ParseRule = struct {
    prefix: ?ParseFn,
    infix: ?ParseFn,
    precedence: Precedence,
};

const rules = init: {
    const count = @typeInfo(s.Token.Type).Enum.fields.len;

    var arr = [_]ParseRule{.{ .prefix = null, .infix = null, .precedence = .NONE }} ** count;
    const t = s.Token.Type;
    arr[@intFromEnum(t.LEFT_PAREN)] = .{ .prefix = Compiler.grouping, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.MINUS)] = .{ .prefix = Compiler.unary, .infix = Compiler.binary, .precedence = .TERM };
    arr[@intFromEnum(t.PLUS)] = .{ .prefix = null, .infix = Compiler.binary, .precedence = .TERM };
    arr[@intFromEnum(t.SLASH)] = .{ .prefix = null, .infix = Compiler.binary, .precedence = .FACTOR };
    arr[@intFromEnum(t.STAR)] = .{ .prefix = null, .infix = Compiler.binary, .precedence = .FACTOR };
    arr[@intFromEnum(t.NUMBER)] = .{ .prefix = Compiler.number, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.TRUE)] = .{ .prefix = Compiler.literal, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.FALSE)] = .{ .prefix = Compiler.literal, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.NIL)] = .{ .prefix = Compiler.literal, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.BANG)] = .{ .prefix = Compiler.unary, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.EQUAL_EQUAL)] = .{ .prefix = null, .infix = Compiler.binary, .precedence = .EQUALITY };
    arr[@intFromEnum(t.BANG_EQUAL)] = .{ .prefix = null, .infix = Compiler.binary, .precedence = .EQUALITY };
    arr[@intFromEnum(t.GREATER)] = .{ .prefix = null, .infix = Compiler.binary, .precedence = .COMPARISON };
    arr[@intFromEnum(t.GREATER_EQUAL)] = .{ .prefix = null, .infix = Compiler.binary, .precedence = .COMPARISON };
    arr[@intFromEnum(t.LESS)] = .{ .prefix = null, .infix = Compiler.binary, .precedence = .COMPARISON };
    arr[@intFromEnum(t.LESS_EQUAL)] = .{ .prefix = null, .infix = Compiler.binary, .precedence = .COMPARISON };
    arr[@intFromEnum(t.STRING)] = .{ .prefix = Compiler.string, .infix = null, .precedence = .NONE };
    arr[@intFromEnum(t.IDENTIFIER)] = .{ .prefix = Compiler.variable, .infix = null, .precedence = .NONE };

    break :init arr;
};

fn get_rule(t: s.Token.Type) ParseRule {
    return rules[@intFromEnum(t)];
}

pub const Compiler = struct {
    current_chunk: ?*common.Chunk = null,
    parser: Parser,
    allocator: std.mem.Allocator,
    vm: *VM,

    pub fn init(vm: *VM, allocator: std.mem.Allocator) Compiler {
        return .{ .parser = .{}, .vm = vm, .allocator = allocator };
    }

    pub fn compile(self: *Compiler, source: []u8, chunk: *common.Chunk) bool {
        self.parser.scanner = s.Scanner.init(source);

        self.parser.had_error = false;
        self.parser.panic_mode = false;

        self.current_chunk = chunk;

        self.advance();
        while (!self.match(.EOF)) {
            self.declaration();
        }

        self.end_compiler();
        return !self.parser.had_error; // TODO: Use errors for this instead of returning a boolean?
    }

    fn advance(self: *Compiler) void {
        self.parser.previous = self.parser.current;

        while (true) {
            self.parser.current = self.parser.scanner.next_token();
            if (self.parser.current.type != .ERROR) break;

            self.error_at_current(self.parser.current.lexeme);
        }
    }

    fn match(self: *Compiler, t: s.Token.Type) bool {
        if (!self.check(t)) return false;
        self.advance();
        return true;
    }

    fn check(self: *Compiler, t: s.Token.Type) bool {
        return self.parser.current.type == t;
    }

    fn consume(self: *Compiler, token_type: s.Token.Type, msg: []const u8) void {
        if (self.parser.current.type == token_type) {
            self.advance();
            return;
        }

        self.error_at_current(msg);
    }

    fn grouping(self: *Compiler, _: bool) void {
        self.expression();
        self.consume(.RIGHT_PAREN, "Expect ')' after expression");
    }

    fn expression(self: *Compiler) void {
        self.parse_precedence(.ASSIGNMENT);
    }

    fn number(self: *Compiler, _: bool) void {
        const val = std.fmt.parseFloat(vals.Number, self.parser.previous.lexeme) catch {
            self.error_at_prev("Can't parse number");
            return;
        };
        self.emit_constant(vals.Value{ .number = val });
    }

    fn unary(self: *Compiler, _: bool) void {
        const opType = self.parser.previous.type;
        self.parse_precedence(.UNARY);

        switch (opType) {
            .MINUS => self.emit_instruction(.OP_NEGATE),
            .BANG => self.emit_instruction(.OP_NOT),
            else => unreachable,
        }
    }

    fn binary(self: *Compiler, _: bool) void {
        const opType = self.parser.previous.type;
        const rule = get_rule(opType);

        self.parse_precedence(@enumFromInt(@intFromEnum(rule.precedence) + 1));

        switch (opType) {
            .PLUS => self.emit_instruction(.OP_ADD),
            .MINUS => self.emit_instruction(.OP_SUBTRACT),
            .STAR => self.emit_instruction(.OP_MULTIPLY),
            .SLASH => self.emit_instruction(.OP_DIVIDE),
            .EQUAL_EQUAL => self.emit_instruction(.OP_EQUAL),
            .BANG_EQUAL => self.emit_instructions(.OP_EQUAL, .OP_NOT),
            .GREATER => self.emit_instruction(.OP_GREATER),
            .GREATER_EQUAL => self.emit_instructions(.OP_LESS, .OP_NOT),
            .LESS => self.emit_instruction(.OP_LESS),
            .LESS_EQUAL => self.emit_instructions(.OP_GREATER, .OP_NOT),

            else => unreachable,
        }
    }

    fn literal(self: *Compiler, _: bool) void {
        switch (self.parser.previous.type) {
            .TRUE => self.emit_instruction(.OP_TRUE),
            .FALSE => self.emit_instruction(.OP_FALSE),
            .NIL => self.emit_instruction(.OP_NIL),
            else => unreachable,
        }
    }

    fn string(self: *Compiler, _: bool) void {
        const source = self.parser.previous.lexeme[1 .. self.parser.previous.lexeme.len - 1];
        const str = objs.copy_string(source, self.vm) catch null; // TODO: Handle allocation errors?
        self.emit_constant(vals.Value{ .obj = str.?.as_obj() });
    }

    fn variable(self: *Compiler, can_assign: bool) void {
        self.named_variable(self.parser.previous, can_assign);
    }

    fn named_variable(self: *Compiler, name: s.Token, can_assign: bool) void {
        const idx = self.identifier_constant(&name);

        if (can_assign and self.match(.EQUAL)) {
            self.expression();
            self.emit_instruction(.OP_SET_GLOBAL);
            self.emit_byte(idx);
        } else {
            self.emit_instruction(.OP_GET_GLOBAL);
            self.emit_byte(idx);
        }
    }

    fn declaration(self: *Compiler) void {
        if (self.match(.VAR)) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if (self.parser.panic_mode) {
            self.synchronize();
        }
    }

    fn var_declaration(self: *Compiler) void {
        const global = self.parse_variable("Expect identifier after var");

        if (self.match(.EQUAL)) {
            self.expression();
        } else {
            self.emit_instruction(.OP_NIL);
        }

        self.consume(.SEMICOLON, "Expect ';' after variable declaration");
        self.emit_instruction(.OP_DEFINE_GLOBAL);
        self.emit_byte(global);
    }

    fn parse_variable(self: *Compiler, error_msg: []const u8) u8 {
        self.consume(.IDENTIFIER, error_msg);
        return self.identifier_constant(&self.parser.previous);
    }

    fn identifier_constant(self: *Compiler, token: *const s.Token) u8 {
        const str = objs.copy_string(token.lexeme, self.vm) catch null; // TODO: Error handling
        return self.make_constant(vals.Value{ .obj = str.?.as_obj() });
    }

    fn synchronize(self: *Compiler) void {
        self.parser.panic_mode = false;

        while (self.parser.current.type != .EOF) {
            if (self.parser.previous.type == .SEMICOLON) break;
            switch (self.parser.current.type) {
                .IF, .FOR, .PRINT, .RETURN, .WHILE, .CLASS, .FUN, .VAR, .BREAK => break,
                else => {},
            }
            self.advance();
        }
    }

    fn statement(self: *Compiler) void {
        if (self.match(.PRINT)) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(self: *Compiler) void {
        self.expression();
        self.emit_instruction(.OP_PRINT);
        self.consume(.SEMICOLON, "Expected ';' after statement");
    }

    fn expression_statement(self: *Compiler) void {
        self.expression();
        self.emit_instruction(.OP_POP);
        self.consume(.SEMICOLON, "Expected ';' after statement");
    }

    fn parse_precedence(self: *Compiler, p: Precedence) void {
        self.advance();
        const can_assign = @intFromEnum(p) <= @intFromEnum(Precedence.ASSIGNMENT);
        const prefix_rule = get_rule(self.parser.previous.type).prefix;

        if (prefix_rule) |f| {
            f(self, can_assign);
        } else {
            self.error_at_prev("Expected expression");
            return;
        }

        while (@intFromEnum(p) <= @intFromEnum(get_rule(self.parser.current.type).precedence)) {
            self.advance();
            const infix_rule = get_rule(self.parser.previous.type).infix;
            if (infix_rule) |f| {
                f(self, can_assign);
            } else {
                self.error_at_prev("No infix rule lul");
                return;
            }
        }

        if (can_assign and self.match(.EQUAL)) {
            self.error_at_prev("Invalid assignment target");
        }
    }

    fn error_at_current(self: *Compiler, msg: []const u8) void {
        self.error_at(&self.parser.current, msg) catch {
            std.debug.print("Can't output error msg\n", .{});
        };
    }

    fn error_at_prev(self: *Compiler, msg: []const u8) void {
        self.error_at(&self.parser.previous, msg) catch {
            std.debug.print("Can't output error msg\n", .{});
        };
    }

    fn error_at(self: *Compiler, token: *s.Token, msg: []const u8) !void {
        if (self.parser.panic_mode) return;

        self.parser.panic_mode = true;
        const stderr = std.io.getStdErr().writer();

        try stderr.print("[line {d}] Error", .{token.line});

        switch (token.type) {
            .EOF => try stderr.print(" at end", .{}),
            .ERROR => {},
            else => try stderr.print(" at '{s}'", .{token.lexeme}),
        }

        try stderr.print(": {s}\n", .{msg});

        self.parser.had_error = true;
    }

    fn emit_byte(self: *Compiler, b: u8) void {
        self.current_chunk.?.write(b, @intCast(self.parser.previous.line)) catch return;
    }

    fn emit_bytes(self: *Compiler, b1: u8, b2: u8) void {
        self.emit_byte(b1);
        self.emit_byte(b2);
    }

    fn emit_instruction(self: *Compiler, t: common.InstructionType) void {
        self.emit_byte(@intFromEnum(t));
    }

    fn emit_instructions(self: *Compiler, t1: common.InstructionType, t2: common.InstructionType) void {
        self.emit_instruction(t1);
        self.emit_instruction(t2);
    }

    fn emit_constant(self: *Compiler, v: vals.Value) void {
        self.emit_bytes(@intFromEnum(common.InstructionType.OP_CONSTANT), self.make_constant(v));
    }

    fn make_constant(self: *Compiler, v: vals.Value) u8 {
        const idx = self.current_chunk.?.add_constant(v);
        if (idx >= std.math.maxInt(u8)) {
            self.error_at_prev("Too many constants in one chunk");
            return 0;
        }

        return @intCast(idx);
    }

    fn end_compiler(self: *Compiler) void {
        self.emit_instruction(.OP_RETURN);
    }
};
