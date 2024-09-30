const std = @import("std");
const s = @import("scanner.zig");

pub fn compile(source: []u8) void {
    var scanner = s.Scanner.init(source);

    var line: i32 = -1;
    while (true) {
        const token = scanner.next_token();
        if (token.type == .EOF) {
            break;
        }

        if (line != token.line) {
            std.debug.print("{d:>4} ", .{token.line});
            line = token.line;
        } else {
            std.debug.print("    | ", .{});
        }

        std.debug.print("{s} '{s}'\n", .{ @tagName(token.type), token.lexeme });
    }
}
