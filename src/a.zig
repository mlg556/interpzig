const std = @import("std");

const Foo = struct {
    x: []const u8,

    pub fn format(self: Foo, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = fmt;
        _ = options;
        return std.fmt.format(writer, "{any}", .{self.x});
    }
};

pub fn main() void {
    var bar = Foo{ .x = "hello" };
    std.debug.print("{}\n", .{bar});
}
