const std = @import("std");
const assert = std.debug.assert;

pub const Token = struct {
    type: Type,
    lexeme: []const u8,
    line: i32,

    pub const Type = enum(u8) {
        LEFT_PAREN,
        RIGHT_PAREN,
        LEFT_BRACE,
        RIGHT_BRACE,
        PLUS,
        MINUS,
        STAR,
        SLASH,
        COMMA,
        DOT,
        EQUAL,
        BANG,
        EQUAL_EQUAL,
        BANG_EQUAL,
        GREATER,
        GREATER_EQUAL,
        LESS,
        LESS_EQUAL,
        NUMBER,
        STRING,
        IDENTIFIER,
        IF,
        ELSE,
        FOR,
        PRINT,
        RETURN,
        WHILE,
        OR,
        AND,
        CLASS,
        FUN,
        NIL,
        THIS,
        SUPER,
        VAR,
        BREAK,
        TRUE,
        FALSE,
        SEMICOLON,
        EOF,
        ERROR,
    };
};

pub const Scanner = struct {
    line: i32,
    source: []const u8,
    start: usize,
    current: usize,

    pub fn init(source: []const u8) Scanner {
        return Scanner{ .line = 1, .source = source, .start = 0, .current = 0 };
    }

    const is_digit = std.ascii.isDigit;
    pub fn next_token(self: *Scanner) Token {
        self.skip_whitespace();
        self.start = self.current;

        if (self.is_at_end()) {
            return self.make_token(.EOF);
        }

        const c = self.advance();
        if (is_digit(c)) {
            return self.number();
        } else if (is_alpha(c)) {
            return self.identifier();
        }

        return switch (c) {
            '(' => self.make_token(.LEFT_PAREN),
            ')' => self.make_token(.RIGHT_PAREN),
            '{' => self.make_token(.LEFT_BRACE),
            '}' => self.make_token(.RIGHT_BRACE),
            '+' => self.make_token(.PLUS),
            '-' => self.make_token(.MINUS),
            '*' => self.make_token(.STAR),
            '/' => self.make_token(.SLASH),
            '!' => self.make_token(if (self.match('=')) .BANG_EQUAL else .BANG),
            '>' => self.make_token(if (self.match('=')) .GREATER_EQUAL else .GREATER),
            '<' => self.make_token(if (self.match('=')) .LESS_EQUAL else .LESS),
            '=' => self.make_token(if (self.match('=')) .EQUAL_EQUAL else .EQUAL),
            ';' => self.make_token(.SEMICOLON),
            '.' => self.make_token(.DOT),
            ',' => self.make_token(.COMMA),
            '"' => self.string(),
            else => self.error_token("Unexpected character"),
        };
    }

    fn skip_whitespace(self: *Scanner) void {
        if (self.is_at_end()) return;

        while (true) {
            switch (self.peek()) {
                '\r', '\t', ' ' => self.current += 1,
                '\n' => {
                    self.current += 1;
                    self.line += 1;
                },
                '/' => {
                    if (self.peek_next() == '/') {
                        while (self.peek() == '/' and !self.is_at_end()) {
                            self.current += 1;
                        }
                    } else {
                        return;
                    }
                },
                else => return,
            }
        }
    }

    fn number(self: *Scanner) Token {
        while (is_digit(self.peek())) self.current += 1;

        if (self.peek() == '.' and is_digit(self.peek_next())) {
            self.current += 1;

            while (is_digit(self.peek())) {
                self.current += 1;
            }
        }

        return self.make_token(.NUMBER);
    }

    fn string(self: *Scanner) Token {
        while (self.peek() != '"') {
            if (self.peek() == '\n') self.line += 1;
            self.current += 1;
        }

        if (self.is_at_end()) {
            return self.error_token("Unterminated string");
        }

        assert(self.peek() == '"');
        self.current += 1;

        return self.make_token(.STRING);
    }

    fn identifier(self: *Scanner) Token {
        while (is_alpha(self.peek()) or is_digit(self.peek())) self.current += 1;
        return self.make_token(self.identifier_type());
    }

    fn identifier_type(self: *Scanner) Token.Type {
        return switch (self.source[self.start]) {
            'a' => self.check_keyword("and", .AND),
            'b' => self.check_keyword("break", .BREAK),
            'c' => self.check_keyword("class", .CLASS),
            'e' => self.check_keyword("else", .ELSE),
            'f' => switch (self.source[self.start + 1]) {
                'a' => self.check_keyword("false", .FALSE),
                'o' => self.check_keyword("for", .FOR),
                'u' => self.check_keyword("fun", .FUN),
                else => .IDENTIFIER,
            },
            'i' => self.check_keyword("if", .IF),
            'n' => self.check_keyword("nil", .NIL),
            'o' => self.check_keyword("or", .OR),
            'p' => self.check_keyword("print", .PRINT),
            'r' => self.check_keyword("return", .RETURN),
            's' => self.check_keyword("super", .SUPER),
            't' => switch (self.source[self.start + 1]) {
                'h' => self.check_keyword("this", .THIS),
                'r' => self.check_keyword("true", .TRUE),
                else => .IDENTIFIER,
            },
            'v' => self.check_keyword("var", .VAR),
            'w' => self.check_keyword("while", .WHILE),
            else => .IDENTIFIER,
        };
    }

    fn check_keyword(self: *Scanner, keyword: []const u8, keyword_type: Token.Type) Token.Type {
        if (std.mem.eql(u8, keyword, self.source[self.start..self.current])) return keyword_type;
        return .IDENTIFIER;
    }

    fn is_at_end(self: *Scanner) bool {
        return self.current == self.source.len;
    }

    fn advance(self: *Scanner) u8 {
        const res = self.source[self.current];
        self.current += 1;
        return res;
    }

    fn peek(self: *Scanner) u8 {
        if (self.is_at_end()) return 0;
        return self.source[self.current];
    }

    fn peek_next(self: *Scanner) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    fn match(self: *Scanner, c: u8) bool {
        if (self.is_at_end()) return false;
        if (self.source[self.current] != c) return false;

        self.current += 1;
        return true;
    }

    fn make_token(self: *Scanner, t: Token.Type) Token {
        return Token{ .type = t, .line = self.line, .lexeme = self.source[self.start..self.current] };
    }

    fn error_token(self: *Scanner, msg: []const u8) Token {
        return Token{ .type = .ERROR, .line = self.line, .lexeme = msg };
    }

    fn is_alpha(c: u8) bool {
        return switch (c) {
            'A'...'Z', 'a'...'z', '_' => true,
            else => false,
        };
    }
};
