const std = @import("std");
const lexer = @import("lexer.zig");

const prompt = ">> ";

pub fn main() !void {
    var reader = std.io.getStdIn().reader();
    var buffer: [1024]u8 = undefined;
    std.debug.print("zonkey v0.1 - 2023\n", .{});
    std.debug.print("{s}", .{prompt});
    while (try reader.readUntilDelimiterOrEof(&buffer, '\n')) |line| {
        var lex = lexer.Lexer.init(line);
        while (lex.ch != 0) {
            const token = lex.nextToken();
            // custom print for token
            switch (token) {
                .IDENT => {
                    std.debug.print(".IDENT = '{s}'\n", .{token.IDENT});
                },
                .INT => {
                    std.debug.print(".INT = '{s}'\n", .{token.INT});
                },
                else => {
                    std.debug.print(".{s}\n", .{@tagName(token)});
                },
            }
        }

        std.debug.print("{s}", .{prompt});
    }
}
