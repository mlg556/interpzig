const std = @import("std");

pub const string = []const u8;

pub const TokenType = enum {
    // only these two have a string payload called "literal"
    IDENT,
    INT,

    ILLEGAL,
    EOF,
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    EQUAL,
    NOT_EQUAL,
    LESS_THAN,
    GREATER_THAN,
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
};

const keyword_map = std.ComptimeStringMap(TokenType, .{
    .{ "fn", .FUNCTION },
    .{ "let", .LET },
    .{ "true", .TRUE },
    .{ "false", .FALSE },
    .{ "if", .IF },
    .{ "else", .ELSE },
    .{ "return", .RETURN },
});

pub const Token = struct {
    type: TokenType = .ILLEGAL,
    literal: string = "",

    pub fn format(self: Token, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        switch (self.literal.len) {
            0 => return std.fmt.format(writer, "{s}", .{@tagName(self.type)}),
            else => return std.fmt.format(writer, "{s}[{s}]", .{ @tagName(self.type), self.literal }),
        }
    }
};

/// Main Lexer struct.
pub const Lexer = struct {
    /// input string line.
    input: string,
    /// current position in input (points to current char)
    position: usize = 0,
    /// current reading position in input (after current char)
    readPosition: usize = 0,
    /// current char under examination
    ch: u8 = 0,

    /// initializes and returns a Lexer.
    pub fn init(input: string) Lexer {
        var lex = Lexer{ .input = input };
        lex.readChar();
        return lex;
    }
    ///  gives us the next character and advance our position in the input string.
    fn readChar(lex: *Lexer) void {
        if (lex.readPosition >= lex.input.len) {
            lex.ch = 0;
        } else {
            lex.ch = lex.input[lex.readPosition];
        }

        lex.position = lex.readPosition;
        lex.readPosition += 1;
    }

    fn readNumber(lex: *Lexer) string {

        // TODO: integrate std.fmt.ParseInt
        const pos = lex.position;

        while (isDigit(lex.ch)) {
            lex.readChar();
        }

        return lex.input[pos..lex.position];
    }

    /// looks at the current character under examination and returns a token depending on which character it is.
    pub fn nextToken(lex: *Lexer) Token {
        lex.skipWhitespace();
        var tok = Token{};
        switch (lex.ch) {
            // '=' and '=='
            '=' => {
                if (lex.peekChar() == '=') {
                    lex.readChar();
                    tok.type = .EQUAL; // '==''
                } else {
                    tok.type = .ASSIGN; // '='
                }
            },
            ';' => tok.type = .SEMICOLON,
            '(' => tok.type = .LPAREN,
            ')' => tok.type = .RPAREN,
            ',' => tok.type = .COMMA,
            '+' => tok.type = .PLUS,
            '{' => tok.type = .LBRACE,
            '}' => tok.type = .RBRACE,
            '-' => tok.type = .MINUS,
            '!' => {
                if (lex.peekChar() == '=') {
                    lex.readChar();
                    tok.type = .NOT_EQUAL;
                } else {
                    tok.type = .BANG;
                }
            },
            '*' => tok.type = .ASTERISK,
            '/' => tok.type = .SLASH,
            '<' => tok.type = .LESS_THAN,
            '>' => tok.type = .GREATER_THAN,

            0 => tok.type = .EOF,

            // isLetter
            'a'...'z', 'A'...'Z', '_' => {
                const ident = lex.readIdentifier();

                if (lookupIdent(ident)) |token_type| {
                    tok.type = token_type;
                    return tok;
                } else {
                    tok.type = .IDENT;
                    tok.literal = ident;
                    return tok;
                }
            },

            // is digit
            '0'...'9' => {
                const num = lex.readNumber();
                tok.type = .INT;
                tok.literal = num;
                return tok;
            },

            else => tok.type = .ILLEGAL,
        }

        lex.readChar();
        return tok;
    }

    fn skipWhitespace(lex: *Lexer) void {
        while (std.ascii.isWhitespace(lex.ch)) {
            lex.readChar();
        }
    }

    /// checks if given char is a letter, ignores "_" so we can use it in identifiers
    fn isLetter(ch: u8) bool {
        return std.ascii.isAlphabetic(ch) or ch == '_';
    }

    fn isDigit(ch: u8) bool {
        return std.ascii.isDigit(ch);
    }

    // returns the next char, 0 if already at the end.
    fn peekChar(lex: Lexer) u8 {
        if (lex.readPosition >= lex.input.len) {
            return 0;
        }

        return lex.input[lex.readPosition];
    }

    /// reads in an identifier and advances the lexer’s positions until it encounters a non-letter-character
    fn readIdentifier(lex: *Lexer) string {
        const pos = lex.position;

        while (isLetter(lex.ch)) {
            lex.readChar();
        }

        return lex.input[pos..lex.position];
    }

    /// if identifier is a keyword, returns a special token type. else returns null
    fn lookupIdent(ident: string) ?TokenType {
        if (keyword_map.get(ident)) |token_type| {
            return token_type;
        }

        return null;
    }
};

test "Lexer" {
    const input = "=+(){},;";
    var lex = Lexer.init(input);

    const exp_tokens = [_]Token{
        .{ .type = .ASSIGN },
        .{ .type = .PLUS },
        .{ .type = .LPAREN },
        .{ .type = .RPAREN },
        .{ .type = .LBRACE },
        .{ .type = .RBRACE },
        .{ .type = .COMMA },
        .{ .type = .SEMICOLON },
        .{ .type = .EOF },
    };

    for (exp_tokens) |exp_token| {
        const token = lex.nextToken();

        try std.testing.expectEqualDeep(exp_token, token);
    }
}

test "Lexer - Full" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\ x + y;
        \\ };
        \\let result = add(five, ten);
        \\!-/*5;
        \\5 < 10 > 5;
        \\if (5 < 10) {
        \\return true;
        \\} else {
        \\return false;
        \\}
        \\10 == 10;
        \\10 != 9;
    ;

    var lex = Lexer.init(input);

    const exp_tokens = [_]Token{
        .{ .type = .LET },
        .{ .type = .IDENT, .literal = "five" },
        .{ .type = .ASSIGN },
        .{ .type = .INT, .literal = "5" },
        .{ .type = .SEMICOLON },
        .{ .type = .LET },
        .{ .type = .IDENT, .literal = "ten" },
        .{ .type = .ASSIGN },
        .{ .type = .INT, .literal = "10" },
        .{ .type = .SEMICOLON },
        .{ .type = .LET },
        .{ .type = .IDENT, .literal = "add" },
        .{ .type = .ASSIGN },
        .{ .type = .FUNCTION },
        .{ .type = .LPAREN },
        .{ .type = .IDENT, .literal = "x" },
        .{ .type = .COMMA },
        .{ .type = .IDENT, .literal = "y" },
        .{ .type = .RPAREN },
        .{ .type = .LBRACE },
        .{ .type = .IDENT, .literal = "x" },
        .{ .type = .PLUS },
        .{ .type = .IDENT, .literal = "y" },
        .{ .type = .SEMICOLON },
        .{ .type = .RBRACE },
        .{ .type = .SEMICOLON },
        .{ .type = .LET },
        .{ .type = .IDENT, .literal = "result" },
        .{ .type = .ASSIGN },
        .{ .type = .IDENT, .literal = "add" },
        .{ .type = .LPAREN },
        .{ .type = .IDENT, .literal = "five" },
        .{ .type = .COMMA },
        .{ .type = .IDENT, .literal = "ten" },
        .{ .type = .RPAREN },
        .{ .type = .SEMICOLON },

        .{ .type = .BANG },
        .{ .type = .MINUS },
        .{ .type = .SLASH },
        .{ .type = .ASTERISK },
        .{ .type = .INT, .literal = "5" },
        .{ .type = .SEMICOLON },
        .{ .type = .INT, .literal = "5" },
        .{ .type = .LESS_THAN },
        .{ .type = .INT, .literal = "10" },
        .{ .type = .GREATER_THAN },
        .{ .type = .INT, .literal = "5" },
        .{ .type = .SEMICOLON },
        .{ .type = .IF },
        .{ .type = .LPAREN },
        .{ .type = .INT, .literal = "5" },
        .{ .type = .LESS_THAN },
        .{ .type = .INT, .literal = "10" },
        .{ .type = .RPAREN },
        .{ .type = .LBRACE },
        .{ .type = .RETURN },
        .{ .type = .TRUE },
        .{ .type = .SEMICOLON },
        .{ .type = .RBRACE },
        .{ .type = .ELSE },
        .{ .type = .LBRACE },
        .{ .type = .RETURN },
        .{ .type = .FALSE },
        .{ .type = .SEMICOLON },
        .{ .type = .RBRACE },

        .{ .type = .INT, .literal = "10" },
        .{ .type = .EQUAL },
        .{ .type = .INT, .literal = "10" },
        .{ .type = .SEMICOLON },

        .{ .type = .INT, .literal = "10" },
        .{ .type = .NOT_EQUAL },
        .{ .type = .INT, .literal = "9" },
        .{ .type = .SEMICOLON },

        .{ .type = .EOF },
    };

    for (exp_tokens) |exp_token| {
        const token = lex.nextToken();
        std.debug.print("{:}\n", .{token});
        try std.testing.expectEqualDeep(exp_token, token);
    }
}
