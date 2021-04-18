const std = @import("std");

const Register = enum(u16) {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    PC,
    COND,
};

const Opcode = enum(u16) {
    BR = 0, // branch
    ADD, // add
    LD, // load
    ST, // store
    JSR, // jump register
    AND, // bitwise and
    LDR, // load register
    STR, // store register
    RTI, // unused
    NOT, // bitwise not
    LDI, // load indirect
    STI, // store indirect
    JMP, // jump
    RES, // reserved (unused)
    LEA, // load effective address
    TRAP // execute trap
};

const Flag = enum(u16) {
    POS = 1 << 0,
    ZRO = 1 << 1,
    NEG = 1 << 2,
};

fn read_image(path: ?[]const u8) anyerror!bool {
    if (path) |p| {
        return true;
    }
    return error.NoPathProvided;
}

var memory = [_]u16{0} ** std.math.maxInt(u16);
var regs = [_]u16{0} ** @typeInfo(Register).Enum.fields.len;

fn get_reg(r: Register) u16 {
    return regs[@enumToInt(r)];
}

fn get_reg_ptr(r: Register) *u16 {
    return &regs[@enumToInt(r)];
}

fn lc3() !void {
    const pc_start = 0x3000;
    get_reg_ptr(.PC).* = pc_start;

    var running = true;
    while (running) {
        var instr = memory[get_reg(.PC)];
        get_reg_ptr(.PC).* += 1;
        var op = instr >> 12;
        switch (@intToEnum(Opcode, op)) {
            .BR => unreachable,
            .ADD => unreachable,
            .LD => unreachable,
            .ST => unreachable,
            .JSR => unreachable,
            .AND => unreachable,
            .LDR => unreachable,
            .STR => unreachable,
            .NOT => unreachable,
            .LDI => unreachable,
            .STI => unreachable,
            .JMP => unreachable,
            .LEA => unreachable,
            .TRAP => unreachable,
            .RTI, .RES => return error.BadOpCode,
        }
        running = false;
    }
}

pub fn main() anyerror!void {
    const al = std.heap.page_allocator;

    const args = try std.process.argsAlloc(al);
    defer std.process.argsFree(al, args);

    if (args.len < 2) {
        std.debug.warn("lc3 [image-file1] ...\n", .{});
        return error.InsufficientArgs;
    }
    // TODO: the catch here isn't detecting that read_image has the return type of `!anything`
    // so it gets mad that we try to catch it. Probably worth a bug report, if consistently reproducable.
    // for (args[1..]) |arg| {
    //     try read_image(arg) catch |e| { // TODO: why doesn't this work?
    //         std.debug.warn("Failed to load file: {}", .{arg});
    //         return e;
    //     };
    // }

    try lc3();
}
