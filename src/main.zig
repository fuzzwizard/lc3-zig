const std = @import("std");

const Program = []u16;

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
    TRAP = 0b1111, // execute trap [1111|0000|TRAPVECT]

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

fn update_flags(r0: u16) void {
    if (reg[r0] == 0) {
        reg_write(.COND, Flag.ZRO.as_u16());
    } else if (reg[r0] >> 15 != 0) {
        reg_write(.COND, Flag.NEG.as_u16());
    } else {
        reg_write(.COND, Flag.POS.as_u16());
    }
}

fn reg_write(r: Register, val: u16) void {
    reg[r.as_u16()] = val;
}

fn reg_read(r: Register) u16 {
    return reg[r.as_u16()];
}

fn mem_read(addr: u16) u16 {
    return memory[addr];
}

fn mem_write(addr: u16, val: u16) void {
    memory[addr] = val;
}

// TODO: fn sign_extend(x: u16, comptime source_type: type) u16;
//    sign_extend(instr, i5) == sign_extend(instr & 0b0001_1111, 5)
//    sign_extend(instr, i6) == sign_extend(instr & 0b0011_1111, 6)
//    sign_extend(instr, i9) == sign_extend(instr & 0b1_1111_1111, 9)
fn sign_extend(x: u16, bit_count: u4) u16 {
    if ((x >> (bit_count - 1)) & 0b1 != 0) {
        return x | @as(u16, 0xFFFF) << bit_count;
    } else {
        return x;
    }
}

fn lc3() !u16 {
    const pc_start = 0x3000;
    reg_write(.PC, pc_start);

    var running = true;
    while (running) {
        var instr = memory[reg_read(.PC)];
        reg_write(.PC, reg_read(.PC) + 1);
        var op = @intToEnum(Opcode, instr >> 12);
        switch (op) {
            // Unused
            .RTI, .RES => return error.BadOpCode,

            // Binary ops
            .ADD => {
                const r0 = (instr >> 9) & 0b111;
                const r1 = (instr >> 6) & 0b111;
                const imm_flag = (instr >> 5) & 1;
                if (imm_flag == 1) {
                    const imm5 = sign_extend(instr & 0b0001_1111, 5);
                    reg[r0] = reg[r1] + imm5;
                } else {
                    const r2 = instr & 0b111;
                    reg[r0] = reg[r1] + reg[r2];
                }
                update_flags(r0);
            },
            .AND => {
                const r0 = (instr >> 9) & 0b111;
                const r1 = (instr >> 6) & 0b111;
                const imm_flag = (instr >> 5) & 1;
                if (imm_flag == 1) {
                    const imm5 = sign_extend(instr & 0b0001_1111, 5);
                    reg[r0] = reg[r1] & imm5;
                } else {
                    const r2 = instr & 0b111;
                    reg[r0] = reg[r1] & reg[r2];
                }
                update_flags(r0);
            },
            .NOT => {
                const r0 = (instr >> 9) & 0b111;
                const r1 = (instr >> 6) & 0b111;
                reg[r0] = ~reg[r1];
                update_flags(r0);
            },

            // Loads
            .LD => {
                const r0 = (instr >> 9) & 0b111;
                const pc_offset = sign_extend(instr & 0b1_1111_1111, 9);
                reg[r0] = mem_read(reg_read(.PC) + pc_offset);
                update_flags(r0);
            },
            .LDR => {
                const r0 = (instr >> 9) & 0b111;
                const r1 = (instr >> 6) & 0b111;
                const offset = sign_extend(instr & 0b0011_1111, 6);
                reg[r0] = mem_read(reg[r1] + offset);
                update_flags(r0);
            },
            .LDI => {
                const r0 = (instr >> 9) & 0b111;
                const pc_offset = sign_extend(instr & 0b1_1111_1111, 9);
                reg[r0] = mem_read(mem_read(reg_read(.PC) + pc_offset));
                update_flags(r0);
            },
            .LEA => {
                const r0 = (instr >> 9) & 0b111;
                const pc_offset = sign_extend(instr & 0b1_1111_1111, 9);
                reg[r0] = reg_read(.PC) + pc_offset;
                update_flags(r0);
            },

            // Stores
            .ST => {
                const r0 = (instr >> 9) & 0b111;
                const pc_offset = sign_extend(instr & 0b1_1111_1111, 9);
                mem_write(reg_read(.PC) + pc_offset, reg[r0]);
            },
            .STR => {
                const r0 = (instr >> 9) & 0b111;
                const r1 = (instr >> 6) & 0b111;
                const offset = sign_extend(instr & 0b0011_1111, 6);
                mem_write(reg[r1] + offset, reg[r0]);
            },
            .STI => {
                const r0 = (instr >> 9) & 0b111;
                const pc_offset = sign_extend(instr & 0b1_1111_1111, 9);
                mem_write(mem_read(reg_read(.PC) + pc_offset), reg[r0]);
            },

            // Jumps, branches
            .BR => {
                const pc_offset = sign_extend(instr & 0b1_1111_1111, 9);
                const cond_flag = (instr >> 9) & 0b111;
                if (cond_flag & reg_read(.COND) != 0) {
                    reg_write(.PC, reg_read(.PC) + pc_offset);
                }
            },
            .JMP => {
                const r1 = (instr >> 6) & 0b111;
                reg_write(.PC, reg[r1]);
            },
            .JSR => {
                const long_flag = (instr >> 11) & 1;
                reg_write(.R7, reg_read(.PC));
                if (long_flag == 1) {
                    const long_pc_offset = sign_extend(0b0000_0111_1111_1111, 11);
                    reg_write(.PC, reg_read(.PC) + long_pc_offset);
                } else {
                    const r1 = (instr >> 6) & 0b111;
                    reg_write(.PC, reg[r1]);
                }
            },

            // Trap codes:
            .TRAP => {},
        }
        running = false;
    }

    return reg_read(.R0);
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
