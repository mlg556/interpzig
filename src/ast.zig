const std = @import("std");

const L = @import("lexer.zig");
const Token = L.Token;
const TokenType = L.TokenType;
const Lexer = L.Lexer;

const string = []const u8;

pub const Expression = struct {};

pub const Identifier = struct {
    token: Token,
    value: string,

    pub fn format(self: Identifier, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;

        return std.fmt.format(writer, "ast.Identifier{{ .token = {}, .value = '{s}' }}", .{ self.token, self.value });
    }
};

pub const Statement = struct { token: Token, name: Identifier, value: Expression };

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn init(alloc: std.mem.Allocator) Program {
        var statements = std.ArrayList(Statement).init(alloc);

        return .{ .statements = statements };
    }
};

pub const Parser = struct {
    ///  A pointer to an instance of the lexer, on which we repeatedly call nextToken() to get the next token in the input.
    lex: *Lexer,
    /// points to the current token
    curToken: Token = Token{ .type = .ILLEGAL, .literal = "" },
    /// points to the next token
    peekToken: Token = Token{ .type = .ILLEGAL, .literal = "" },
    /// number of errors
    errCount: u32 = 0,

    /// initializes parser with a given lexer and allocator.
    pub fn init(l: *Lexer) Parser {
        var p: Parser = .{ .lex = l };

        // Read two tokens, so curToken and peekToken are both set
        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn nextToken(p: *Parser) void {
        p.curToken = p.peekToken;
        p.peekToken = p.lex.nextToken();
    }
    /// constructs the root node of the AST, an *ast.Program. iterates over every token in the input until it encounters an token.EOF.. In every iteration it calls parseStatement, whose job it is to parse a statement. If parseStatement returned something other than nil, a ast.Statement, its return value is added to Statements list of the AST root node. When nothing is left to parse the *ast.Program root node is returned.
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

    /// The expectPeek method is one of the “assertion functions” nearly all parsers share. Their primary purpose is to enforce the correctness of the order of tokens by checking the type of the next token. Our expectPeek here checks the type of the peekToken and only if the type is correct does it advance the tokens by calling nextToken.
    pub fn expectPeek(p: *Parser, t: TokenType) bool {
        if (p.peekTokenIs(t)) {
            p.nextToken();
            return true;
        }
        p.peekError(t);
        return false;
    }

    // Instead of allocating an arraylist for errors, we just hold an error counter and debug.print them for now.
    pub fn peekError(p: *Parser, t: TokenType) void {
        // increment error count
        p.errCount += 1;
        std.debug.print("ERR: expected token {s}, got {s}.", .{ @tagName(t), @tagName(p.peekToken.type) });
    }
};

test "AST" {
    const input =
        \\let x = 3 ;
        // \\let ya = 53;
    ;

    // zig fmt: off
    const exp_stmts = [_]Statement{
        Statement{
            .token = Token{ .type = .LET, .literal = "" },
            .name = Identifier{ .token = Token{ .type = .IDENT, .literal = "x" }, .value = "x"},
            .value = .{} 
        },
        Statement{
            .token = Token{ .type = .LET, .literal = "" },
            .name = Identifier{ .token = Token{ .type = .IDENT, .literal = "ya" }, .value = "ya"},
            .value = .{} 
        },

    };
    // zig fmt: on

    var buffer: [1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const allocator = fba.allocator();

    var lex = Lexer.init(input);
    var parser = Parser.init(&lex);
    var prog = try parser.parseProgram(allocator);

    std.debug.print("\n", .{});

    // no peek errors
    try std.testing.expect(parser.errCount == 0);

    for (prog.statements.items, 0..) |stmt, i| {
        // std.debug.print("{:}\n", .{stmt});

        try std.testing.expectEqualDeep(exp_stmts[i], stmt);
        //try std.json.stringify(stmt, .{}, std.io.getStdOut().writer());
    }
}
