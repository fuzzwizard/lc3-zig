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

pub fn main() anyerror!void {
    var memory = [_]u16{0} ** std.math.maxInt(u16);
    var regs = [_]u16{0} ** @typeInfo(Register).Enum.fields.len;

    const pc_start = 0x300;
    regs[@enumToInt(Register.PC)] = pc_start;

    var running = true;
    while (running) {
        var instr = memory[regs[@enumToInt(Register.PC)]];
        regs[@enumToInt(Register.PC)] += 1;
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
            .RTI => unreachable,
            .NOT => unreachable,
            .LDI => unreachable,
            .STI => unreachable,
            .JMP => unreachable,
            .RES => unreachable,
            .LEA => unreachable,
            .TRAP => unreachable,
            else => {},
        }
        running = false;
    }
}
