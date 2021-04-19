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

    const Self = @This();

    pub fn as_u16(self: Self) u16 {
        return @enumToInt(self);
    }

    pub fn get_count() usize {
        return @typeInfo(Register).Enum.fields.len;
    }
};

const Opcode = enum(u16) {
    BR = 0b0000, // branch
    ADD = 0b0001, // add:
    //  [0001|DR |SR1|0|00|SR2] (register)
    //  [0001|DR |SR1|1| IMM5 ] (immediate s5 value)
    LD = 0b0010, // load
    ST = 0b0011, // store
    JSR = 0b0100, // jump register
    AND = 0b0101, // bitwise and
    LDR = 0b0110, // load register
    STR = 0b0111, // store register
    RTI = 0b1000, // unused
    NOT = 0b1001, // bitwise not
    LDI = 0b1010, // load indirect [1010|DR |OFFSET   ]
    STI = 0b1011, // store indirect
    JMP = 0b1100, // jump
    RES = 0b1101, // reserved (unused)
    LEA = 0b1110, // load effective address
    TRAP = 0b1111, // execute trap

    const Self = @This();
    pub fn as_u16(self: Self) u16 {
        return @enumToInt(self);
    }
};

const Flag = enum(u16) {
    POS = 1 << 0,
    ZRO = 1 << 1,
    NEG = 1 << 2,

    const Self = @This();
    pub fn as_u16(self: Self) u16 {
        return @enumToInt(self);
    }
};

fn read_image(path: ?[]const u8) !void {
    if (path) |p| {
        // UNIMPLEMENTED
        return;
    }
    return error.NoPathProvided;
}

var memory = [_]u16{0} ** std.math.maxInt(u16);
var reg = [_]u16{0} ** Register.get_count();

inline fn update_flags(r0: u16) void {
    if (reg[r] == 0) {
        set_reg(.COND, Flag.ZRO.as_u16());
    } else if (reg[r] >> 15 != 0) {
        set_reg(.COND, Flag.NEG.as_u16());
    } else {
        set_reg(.COND, Flag.POS.as_u16());
    }
}

inline fn set_reg(r: Register, val: u16) void {
    reg[r.as_u16()] = val;
}

inline fn get_reg(r: Register) u16 {
    return reg[r.as_u16()];
}

inline fn mem_read(addr: u16) u16 {
    return memory[addr];
}

inline fn sign_extend(x: u16, bit_count: u4) u16 {
    if ((x >> (bit_count - 1)) & 0b1 != 0) {
        return x | @as(u16, 0xFFFF) << bit_count;
    } else {
        return x;
    }
}

fn lc3() !u16 {
    const pc_start = 0x3000;
    set_reg(.PC, pc_start);

    var running = true;
    while (running) {
        var instr = memory[get_reg(.PC)];
        set_reg(.PC, get_reg(.PC) + 1);
        var op = @intToEnum(Opcode, instr >> 12);
        switch (op) {
            .ADD => {
                const r0 = (instr >> 9) & 0x7;
                const r1 = (instr >> 6) & 0x7;
                const imm_flag = (instr >> 5) & 0x1;
                if (imm_flag == 1) {
                    const imm5 = sign_extend(instr & 0b1_1111, 5);
                    reg[r0] = reg[r1] + imm5;
                } else {
                    const r2 = instr & 0x7;
                    reg[r0] = reg[r1] + reg[r2];
                }
                update_flags(r0);
            },
            .LDI => {
                const r0 = (instr >> 9) & 0x7;
                const pc_offset = sign_extend(instr & 0b1_1111_1111, 9);
                reg[r0] = mem_read(mem_read(get_reg(.PC) + pc_offset));
                update_flags(r0);
            },
            .BR => {},
            .LD => {},
            .ST => {},
            .JSR => {},
            .AND => {},
            .LDR => {},
            .STR => {},
            .NOT => {},
            .STI => {},
            .JMP => {},
            .LEA => {},
            .TRAP => {},
            .RTI, .RES => return error.BadOpCode,
        }
        running = false;
    }

    return get_reg(.R0);
}

pub fn main() anyerror!void {
    const al = std.heap.page_allocator;

    const args = try std.process.argsAlloc(al);
    defer std.process.argsFree(al, args);

    if (args.len < 2) {
        std.debug.warn("lc3 [image-file1] ...\n", .{});
        return error.InsufficientArgs;
    }

    // for (args[1..]) |arg| {
    //     try read_image(arg) catch |e| {
    // TODO: the catch here isn't detecting that read_image has the return type of `!void`
    // so it gets mad that we try to catch it. Probably worth a bug report, if consistently reproducable. (Zig 7.1.0)
    //         std.debug.warn("Failed to load file: {}", .{arg});
    //         return e;
    //     };
    // }

    _ = try lc3();
}

test "smoke" {
    _ = try lc3();
}
