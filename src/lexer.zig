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
    COMMA,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    FUNCTION,
    LET,
};

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
    pub fn readChar(lex: *Lexer) void {
        if (lex.readPosition >= lex.input.len) {
            lex.ch = 0;
        } else {
            lex.ch = lex.input[lex.readPosition];
        }

        lex.position = lex.readPosition;
        lex.readPosition += 1;
    }

    /// looks at the current character under examination and returns a token depending on which character it is.
    pub fn nextToken(lex: *Lexer) Token {
        const tok: Token = switch (lex.ch) {
            '=' => .ASSIGN,
            ';' => .SEMICOLON,
            '(' => .LPAREN,
            ')' => .RPAREN,
            ',' => .COMMA,
            '+' => .PLUS,
            '{' => .LBRACE,
            '}' => .RBRACE,
            0 => .EOF,
            else => {
                if (isLetter(lex.ch)) {
                    .{ .IDENT = lex.readIdentifier() };
                } else {
                    .ILLEGAL;
                }
            },
        };

        lex.readChar();
        return tok;
    }

    /// checks if given char is a letter, ignores "_" so we can use it in identifiers
    pub fn isLetter(ch: u8) bool {
        return std.ascii.isAlphabetic(ch) or ch == '_';
    }

    /// reads in an identifier and advances the lexer’s positions until it encounters a non-letter-character
    pub fn readIdentifier(lex: *Lexer) string {
        const pos = lex.position;

        while (isLetter(lex.ch)) {
            lex.readChar();
        }

        return lex.input[pos..lex.position];
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
