const std = @import("std");

const lex = @import("lexer.zig");
const Token = lex.Token;
const Lexer = lex.Lexer;
const string = lex.string;

// a program can consist of this many statements? no heap?
pub const N = 100;

pub const Expression = struct {};

pub const Identifier = struct { token: Token, value: string };

pub const Statement = struct { token: Token, name: Identifier, value: Expression };

pub const Program = struct { statements: std.ArrayList(Statement) };

pub const Parser = struct {
    lex: *Lexer,
    curToken: Token,
    peekToken: Token,

    pub fn init(l: *Lexer) Parser {
        var p = .{
            .lex = l,
        };

        // Read two tokens, so curToken and peekToken are both set
        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn nextToken(p: *Parser) void {
        p.curToken = p.peekToken;
        p.peekToken = p.lex.nextToken();
    }

    pub fn parseProgram(p: *Parser, alloc: std.mem.Allocator) *Program {
        // program is an arraylist of statements, we keep lexing and pushing
        var program: Program = .{ .statements = std.ArrayList(Statement).init(alloc) };
        _ = program;

        while (!p.curToken.isEOF()) {}
    }
};
