// so we could do tokens this way:
// pub const TokenType = string;
// pub const Token = struct { Type: TokenType, Literal: string };
// but the "Literal" "payload" is only needed in
// "identifier", to get its name, and "int", to get its value.
// so lets use an union(enum), and lets not carry chars around, also its more cooler apparently.
// refresher: A bare union defines a set of possible types that a value can be as a list of fields. Only one field can be active at a time. Unions can be declared with an enum tag type. This turns the union into a tagged union, which makes it eligible to use with switch expressions Unions can be made to infer the enum tag type.

const std = @import("std");

pub const string = []const u8; // type alias

/// Type for holding the information about a  IDENT and INT have string payloads corresponding to their values.
pub const Token = union(enum) {
    IDENT: string,
    INT: string,

    ILLEGAL,
    EOF,
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,
    LT,
    GT,
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    EQ,
    NOT_EQ,

    // keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
};

const keyword_map = std.ComptimeStringMap(Token, .{
    .{ "fn", .FUNCTION },
    .{ "let", .LET },
    .{ "true", .TRUE },
    .{ "false", .FALSE },
    .{ "if", .IF },
    .{ "else", .ELSE },
    .{ "return", .RETURN },
});

/// Main Lexer struct.
const Lexer = struct {
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
        var pos = lex.position;

        while (isDigit(lex.ch)) {
            lex.readChar();
        }

        return lex.input[pos..lex.position];
    }

    /// looks at the current character under examination and returns a token depending on which character it is.
    pub fn nextToken(lex: *Lexer) Token {
        lex.skipWhitespace();
        const tok: Token = switch (lex.ch) {
            // '=' and '=='
            '=' => blk: {
                if (lex.peekChar() == '=') {
                    lex.readChar();
                    break :blk .EQ; // '==''
                }
                break :blk .ASSIGN; // '='
            },
            ';' => .SEMICOLON,
            '(' => .LPAREN,
            ')' => .RPAREN,
            ',' => .COMMA,
            '+' => .PLUS,
            '{' => .LBRACE,
            '}' => .RBRACE,
            '-' => .MINUS,
            '!' => blk: {
                if (lex.peekChar() == '=') {
                    lex.readChar();
                    break :blk .NOT_EQ;
                }
                break :blk .BANG;
            },
            '*' => .ASTERISK,
            '/' => .SLASH,
            '<' => .LT,
            '>' => .GT,

            0 => .EOF,

            // isLetter
            'a'...'z', 'A'...'Z', '_' => {
                // read identifier, could be keyword or not
                const ident = lex.readIdentifier();

                // if keyword, return its token
                if (lookupIdent(ident)) |token| {
                    return token;
                }

                // if not, return an identifier token with payload
                return .{ .IDENT = ident };
            },

            // is digit
            '0'...'9' => {
                const num = lex.readNumber();
                return .{ .INT = num };
            },

            else => .ILLEGAL,
        };

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

    /// checks the keywords table to see whether the given identifier is in fact a keyword. returns is as a token if so. else returns null.
    fn lookupIdent(ident: string) ?Token {
        return keyword_map.get(ident);
    }
};

test "Lexer" {
    const input = "=+(){},;";
    var lex = Lexer.init(input);

    const exp_tokens = [_]Token{
        .ASSIGN,
        .PLUS,
        .LPAREN,
        .RPAREN,
        .LBRACE,
        .RBRACE,
        .COMMA,
        .SEMICOLON,
        .EOF,
    };

    for (exp_tokens) |exp_token| {
        const token = lex.nextToken();

        try std.testing.expectEqualDeep(exp_token, token);
    }
}

test "Lexer - More" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\let add = fn(x, y) {
        \\    x + y;
        \\};
        \\let result = add(five, ten);
    ;

    var lex = Lexer.init(input);

    const exp_tokens = [_]Token{
        .LET,
        .{ .IDENT = "five" }, // notice the identifier with payload "five"
        .ASSIGN,
        .{ .INT = "5" },
        .SEMICOLON,
        .LET,
        .{ .IDENT = "ten" },
        .ASSIGN,
        .{ .INT = "10" },
        .SEMICOLON,
        .LET,
        .{ .IDENT = "add" },
        .ASSIGN,
        .FUNCTION,
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

    var exp_tokens = [_]Token{
        .LET,
        .{ .IDENT = "five" },
        .ASSIGN,
        .{ .INT = "5" },
        .SEMICOLON,
        .LET,
        .{ .IDENT = "ten" },
        .ASSIGN,
        .{ .INT = "10" },
        .SEMICOLON,
        .LET,
        .{ .IDENT = "add" },
        .ASSIGN,
        .FUNCTION,
        .LPAREN,
        .{ .IDENT = "x" },
        .COMMA,
        .{ .IDENT = "y" },
        .RPAREN,
        .LBRACE,
        .{ .IDENT = "x" },
        .PLUS,
        .{ .IDENT = "y" },
        .SEMICOLON,
        .RBRACE,
        .SEMICOLON,
        .LET,
        .{ .IDENT = "result" },
        .ASSIGN,
        .{ .IDENT = "add" },
        .LPAREN,
        .{ .IDENT = "five" },
        .COMMA,
        .{ .IDENT = "ten" },
        .RPAREN,
        .SEMICOLON,

        .BANG,
        .MINUS,
        .SLASH,
        .ASTERISK,
        .{ .INT = "5" },
        .SEMICOLON,
        .{ .INT = "5" },
        .LT,
        .{ .INT = "10" },
        .GT,
        .{ .INT = "5" },
        .SEMICOLON,
        .IF,
        .LPAREN,
        .{ .INT = "5" },
        .LT,
        .{ .INT = "10" },
        .RPAREN,
        .LBRACE,
        .RETURN,
        .TRUE,
        .SEMICOLON,
        .RBRACE,
        .ELSE,
        .LBRACE,
        .RETURN,
        .FALSE,
        .SEMICOLON,
        .RBRACE,

        .{ .INT = "10" },
        .EQ,
        .{ .INT = "10" },
        .SEMICOLON,

        .{ .INT = "10" },
        .NOT_EQ,
        .{ .INT = "9" },
        .SEMICOLON,

        .EOF,
    };

    for (exp_tokens) |exp_token| {
        const token = lex.nextToken();
        std.debug.print("{:}\n", .{token});
        try std.testing.expectEqualDeep(exp_token, token);
    }
}
