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

const Local = struct {
    name: s.Token,
    depth: i32,
};

pub const MAX_LOCALS = std.math.maxInt(u8) + 1;

fn identifiers_equal(ident1: s.Token, ident2: s.Token) bool {
    return std.mem.eql(u8, ident1.lexeme, ident2.lexeme);
}

pub const Compiler = struct {
    allocator: std.mem.Allocator,
    parser: Parser,
    locals: [MAX_LOCALS]Local,
    locals_cnt: usize,
    scope_depth: i32,
    current_chunk: ?*common.Chunk = null,
    vm: *VM,

    pub fn init(vm: *VM, allocator: std.mem.Allocator) Compiler {
        return .{ .parser = .{}, .vm = vm, .allocator = allocator, .locals = undefined, .locals_cnt = 0, .scope_depth = 0 };
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
        var idx = self.resolve_local(name);
        var get_op: common.InstructionType = undefined;
        var set_op: common.InstructionType = undefined;
        if (idx != -1) {
            get_op = .OP_GET_LOCAL;
            set_op = .OP_SET_LOCAL;
        } else {
            idx = self.identifier_constant(&name);
            get_op = .OP_GET_GLOBAL;
            set_op = .OP_SET_GLOBAL;
        }

        if (can_assign and self.match(.EQUAL)) {
            self.expression();
            self.emit_instruction(set_op);
            self.emit_byte(@intCast(idx));
        } else {
            self.emit_instruction(get_op);
            self.emit_byte(@intCast(idx));
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
        self.define_variable(global);
    }

    fn parse_variable(self: *Compiler, error_msg: []const u8) u8 {
        self.consume(.IDENTIFIER, error_msg);

        self.declare_variable();
        if (self.scope_depth > 0) return 0;

        return self.identifier_constant(&self.parser.previous);
    }

    fn declare_variable(self: *Compiler) void {
        if (self.scope_depth == 0) return;

        const name = &self.parser.previous;

        if (self.locals_cnt > 0) {
            var i = self.locals_cnt - 1;
            while (i >= 0) : (i -= 1) {
                if (self.locals[i].depth != -1 and self.locals[i].depth < self.scope_depth) {
                    break;
                }

                if (identifiers_equal(self.locals[i].name, name.*)) {
                    const msg = std.fmt.allocPrint(self.allocator, "Variable '{s}' is already defined in this scope", .{name.lexeme}) catch return;
                    self.error_at_prev(msg);
                }
            }
        }

        self.add_local(name.*);
    }

    fn define_variable(self: *Compiler, global: u8) void {
        if (self.scope_depth > 0) {
            self.mark_initialized();
            return;
        }

        self.emit_instruction(.OP_DEFINE_GLOBAL);
        self.emit_byte(global);
    }

    fn mark_initialized(self: *Compiler) void {
        self.locals[self.locals_cnt - 1].depth = self.scope_depth;
    }

    fn add_local(self: *Compiler, token: s.Token) void {
        if (self.locals_cnt + 1 > MAX_LOCALS) {
            self.err("Too many locals in scope");
            return;
        }

        self.locals[self.locals_cnt] = .{ .name = token, .depth = -1 };
        self.locals_cnt += 1;
    }

    fn resolve_local(self: *Compiler, token: s.Token) i32 {
        var idx: usize = 0;
        while (idx < self.locals_cnt) : (idx += 1) {
            if (identifiers_equal(self.locals[idx].name, token)) {
                if (self.locals[idx].depth == -1) {
                    self.err("Can't refer to a variable in its own initializer");
                }
                return @intCast(idx);
            }
        }

        return -1;
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
        } else if (self.match(.LEFT_BRACE)) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if (self.match(.IF)) {
            self.if_statement();
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

    fn block(self: *Compiler) void {
        while (!self.check(.RIGHT_BRACE) and !self.check(.EOF)) {
            self.declaration();
        }

        self.consume(.RIGHT_BRACE, "Expect '}' after block");
    }

    fn begin_scope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn end_scope(self: *Compiler) void {
        self.scope_depth -= 1;

        while (self.locals_cnt > 0 and self.locals[self.locals_cnt - 1].depth > self.scope_depth) {
            self.emit_instruction(.OP_POP);
            self.locals_cnt -= 1;
        }
    }

    fn if_statement(self: *Compiler) void {
        self.consume(.LEFT_PAREN, "Expected '(' after if");
        self.expression();
        self.consume(.RIGHT_PAREN, "Expected ')' after if condition");

        const elseJump = self.emit_jump(.OP_JUMP_IF_FALSE);
        self.statement();

        self.patch_jump(elseJump);
    }

    fn parse_precedence(self: *Compiler, p: Precedence) void {
        self.advance();
        const can_assign = @intFromEnum(p) <= @intFromEnum(Precedence.ASSIGNMENT);
        const prefix_rule = get_rule(self.parser.previous.type).prefix;

        if (prefix_rule) |f| {
            f(self, can_assign);
        } else {
            self.err("Expected expression");
            return;
        }

        while (@intFromEnum(p) <= @intFromEnum(get_rule(self.parser.current.type).precedence)) {
            self.advance();
            const infix_rule = get_rule(self.parser.previous.type).infix;
            if (infix_rule) |f| {
                f(self, can_assign);
            } else {
                self.err("No infix rule lul");
                return;
            }
        }

        if (can_assign and self.match(.EQUAL)) {
            self.err("Invalid assignment target");
        }
    }

    fn err(self: *Compiler, msg: []const u8) void {
        self.error_at_prev(msg);
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

    fn emit_jump(self: *Compiler, t: common.InstructionType) usize {
        self.emit_instruction(t);
        self.emit_bytes(0xff, 0xff);

        return self.current_chunk.?.code.items.len - 2;
    }

    fn patch_jump(self: *Compiler, offset: usize) void {
        const to_jump: usize = self.current_chunk.?.code.items.len - offset - 2; // -2 since we don't need to "jump" over the bytes for the offset (they're read anyway)

        // TODO: Handle case when to_jump does not fit it u16
        self.current_chunk.?.code.items[offset] = @intCast((to_jump >> 8) & 0xff);
        self.current_chunk.?.code.items[offset + 1] = @intCast(to_jump & 0xff);
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
