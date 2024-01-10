const std = @import("std");

const L = @import("lexer.zig");
const Token = L.Token;
const TokenType = L.TokenType;
const Lexer = L.Lexer;

const string = []const u8;

pub const Expression = struct {};

pub const Identifier = struct { token: Token, value: string };

pub const Statement = struct { token: Token, name: Identifier, value: Expression };

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn init(alloc: std.mem.Allocator) Program {
        var statements = std.ArrayList(Statement).init(alloc);

        return .{ .statements = statements };
    }
};

pub const Parser = struct {
    lex: *Lexer,
    curToken: Token,
    peekToken: Token,

    pub fn init(l: *Lexer) Parser {
        var p: Parser = .{ .lex = l, .curToken = undefined, .peekToken = undefined };

        // Read two tokens, so curToken and peekToken are both set
        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn nextToken(p: *Parser) void {
        p.curToken = p.peekToken;
        p.peekToken = p.lex.nextToken();
    }

    pub fn parseProgram(p: *Parser, alloc: std.mem.Allocator) !*Program {
        // program is an arraylist of statements, we keep lexing and pushing
        var program = Program.init(alloc);

        while (!p.curTokenIs(TokenType.EOF)) {
            var stmt = p.parseStatement();

            if (stmt) |statement| {
                try program.statements.append(statement);
            }

            p.nextToken();
        }

        return &program;
    }

    pub fn parseStatement(p: *Parser) ?Statement {
        switch (p.curToken.type) {
            .LET => return p.parseLetStatement(),
            else => return null,
        }
    }

    pub fn parseLetStatement(p: *Parser) ?Statement {
        var stmt = Statement{ .token = p.curToken, .name = undefined, .value = undefined };

        if (!p.expectPeek(TokenType.IDENT))
            return null;

        stmt.name = .{ .token = p.curToken, .value = p.curToken.literal };

        if (!p.expectPeek(TokenType.ASSIGN))
            return null;

        // TODO: We're skipping the expressions until we
        // encounter a semicolon
        while (!p.curTokenIs(TokenType.SEMICOLON)) {
            p.nextToken();
        }

        return stmt;
    }

    pub fn curTokenIs(p: *Parser, t: TokenType) bool {
        return p.curToken.type == t;
    }

    pub fn peekTokenIs(p: *Parser, t: TokenType) bool {
        return p.peekToken.type == t;
    }

    pub fn expectPeek(p: *Parser, t: TokenType) bool {
        if (p.peekTokenIs(t)) {
            p.nextToken();
            return true;
        }
        return false;
    }
};

test "AST" {
    var buffer: [1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    const input = "let x = 5;";
    var lex = Lexer.init(input);
    var parser = Parser.init(&lex);
    var prog = try parser.parseProgram(allocator);

    for (prog.statements.items) |stmt| {
        std.debug.print("{:}\n", .{stmt});
    }
}
