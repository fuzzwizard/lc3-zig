const std = @import("std");
const mem = std.mem;
const math = std.math;
const io = std.io;
const fs = std.fs;

const Register = enum(u16) {
    R0 = 0b0000,
    R1 = 0b0001,
    R2 = 0b0010,
    R3 = 0b0011,
    R4 = 0b0100,
    R5 = 0b0101,
    R6 = 0b0110,
    R7 = 0b0111,
    PC = 0b1000, // program counter
    COND = 0b1001, // conditional flags
};

const Opcode = enum(u16) {
    BR = 0b0000, // branch
    ADD = 0b0001, // add
    LD = 0b0010, // load
    ST = 0b0011, // store
    JSR = 0b0100, // jump register
    AND = 0b0101, // bitwise and
    LDR = 0b0110, // load register
    STR = 0b0111, // store register
    RTI = 0b1000, // unused
    NOT = 0b1001, // bitwise not
    LDI = 0b1010, // load indirect
    STI = 0b1011, // store indirect
    JMP = 0b1100, // jump
    RES = 0b1101, // reserved (unused)
    LEA = 0b1110, // load effective address
    TRAP = 0b1111, // execute trap
};

const Flag = enum(u16) {
    POS = 0b001,
    ZRO = 0b010,
    NEG = 0b100,
};

const Trap = enum(u16) {
    GETC = 0x20, // get character from keyboard, not echoed onto the terminal
    OUT = 0x21, // output a character
    PUTS = 0x22, // output a word string
    IN = 0x23, // get character from keyboard, echoed onto the terminal
    PUTSP = 0x24, // output a byte string
    HALT = 0x25 // halt the program
};

var memory = [_]u16{0} ** math.maxInt(u16);
var reg = [_]u16{0} ** @typeInfo(Register).Enum.fields.len;

fn update_flags(r0: u16) void {
    if (reg[r0] == 0) { // TODO: do we need to account for -0?
        reg_write(.COND, @enumToInt(Flag.ZRO));
    } else if (reg[r0] >> 15 != 0) {
        reg_write(.COND, @enumToInt(Flag.NEG));
    } else {
        reg_write(.COND, @enumToInt(Flag.POS));
    }
}

fn reg_write(r: Register, val: u16) void {
    reg[@enumToInt(r)] = val;
}

fn reg_read(r: Register) u16 {
    return reg[@enumToInt(r)];
}

fn mem_read(addr: u16) u16 {
    return memory[addr];
}

fn mem_write(addr: u16, val: u16) void {
    memory[addr] = val;
}

fn read_image(path: []const u8) !void {
    const fd = try fs.Dir.openFile(path, .{});
    try read_image_file(fd);
}

fn read_image_file(f: io.File) !void {
    const file = f.reader();
    const origin = mem.nativeToBig(try file.readIntLittle(u16));
    const max_read = math.maxInt(u16) - origin;
    var span = memory[origin .. origin + max_read];
    _ = file.readNoEof(mem.asBytes(span));
    for (span) |*word| {
        word.* = mem.nativeToBig(word.*);
    }
}

// TODO: fn sign_extend(x: u16, comptime source_type: type) u16;
//    sign_extend2(instr, 5) == sign_extend(instr & 0b0001_1111, 5)
//    sign_extend(instr, 6) == sign_extend(instr & 0b0011_1111, 6)
//    sign_extend(instr, 9) == sign_extend(instr & 0b1_1111_1111, 9)
// fn sign_extend_2(instr: u16, comptime sz: usize) u16 {
//     var ret = instr & ~(@as(u16, 0xFFFF) << sz);
//     if ((ret >> (sz - 1)) & 1 != 0) {
//         return ret | (@as(u16, 0xFFFF) << sz);
//     } else {
//         return ret;
//     }
// }
fn sign_extend(x: u16, bit_count: u4) u16 {
    if ((x >> (bit_count - 1)) & 1 != 0) {
        return x | @as(u16, 0xFFFF) << bit_count;
    } else {
        return x;
    }
}

fn lc3() !u16 {
    const stdout = io.getStdOut().writer();
    const stdin = io.getStdIn().reader();

    const pc_start = 0x3000;
    reg_write(.PC, pc_start);

    var running = true;
    while (running) {
        var instr = memory[reg_read(.PC)];
        reg_write(.PC, reg_read(.PC) + 1);
        var op = @intToEnum(Opcode, instr >> 12);
        switch (op) {
            // Unused
            .RTI, .RES => return error.BadOpcode,

            // Binary ops
            .ADD => {
                const r0 = (instr >> 9) & 0b111;
                const r1 = (instr >> 6) & 0b111;
                const imm_flag = (instr >> 5) & 1;
                if (imm_flag == 1) {
                    const imm5 = sign_extend(instr & 0x1F, 5);
                    reg[r0] = reg[r1] +% imm5; // TODO: what's the overflow behaviour on lc3?
                } else {
                    const r2 = instr & 0b111;
                    reg[r0] = reg[r1] +% reg[r2];
                }
                update_flags(r0);
            },
            .AND => {
                const r0 = (instr >> 9) & 0b111;
                const r1 = (instr >> 6) & 0b111;
                const imm_flag = (instr >> 5) & 1;
                if (imm_flag == 1) {
                    const imm5 = sign_extend(instr & 0x1F, 5);
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
                const pc_offset = sign_extend(instr & 0x1FF, 9);
                reg[r0] = mem_read(reg_read(.PC) + pc_offset);
                update_flags(r0);
            },
            .LDR => {
                const r0 = (instr >> 9) & 0b111;
                const r1 = (instr >> 6) & 0b111;
                const offset = sign_extend(instr & 0x3F, 6);
                reg[r0] = mem_read(reg[r1] + offset);
                update_flags(r0);
            },
            .LDI => {
                const r0 = (instr >> 9) & 0b111;
                const pc_offset = sign_extend(instr & 0x1FF, 9);
                reg[r0] = mem_read(mem_read(reg_read(.PC) + pc_offset));
                update_flags(r0);
            },
            .LEA => {
                const r0 = (instr >> 9) & 0b111;
                const pc_offset = sign_extend(instr & 0x1FF, 9);
                reg[r0] = reg_read(.PC) + pc_offset;
                update_flags(r0);
            },

            // Stores
            .ST => {
                const r0 = (instr >> 9) & 0b111;
                const pc_offset = sign_extend(instr & 0x1FF, 9);
                mem_write(reg_read(.PC) + pc_offset, reg[r0]);
            },
            .STR => {
                const r0 = (instr >> 9) & 0b111;
                const r1 = (instr >> 6) & 0b111;
                const offset = sign_extend(instr & 0x3F, 6);
                mem_write(reg[r1] + offset, reg[r0]);
            },
            .STI => {
                const r0 = (instr >> 9) & 0b111;
                const pc_offset = sign_extend(instr & 0x1FF, 9);
                mem_write(mem_read(reg_read(.PC) + pc_offset), reg[r0]);
            },

            // Jumps, branches
            .BR => {
                const pc_offset = sign_extend(instr & 0x1FF, 9);
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
                    const long_pc_offset = sign_extend(0x3FF, 11);
                    reg_write(.PC, reg_read(.PC) + long_pc_offset);
                } else {
                    const r1 = (instr >> 6) & 0b111;
                    reg_write(.PC, reg[r1]);
                }
            },

            // Trap codes:
            .TRAP => {
                var code = @intToEnum(Trap, instr & 0xFF);
                switch (code) {
                    .IN => {
                        const ch8 = try stdin.readIntNative(u8);
                        reg_write(.R0, @intCast(u16, ch8));
                        try stdout.writeByte(ch8);
                    },
                    .OUT => {
                        const ch8 = try math.cast(u8, reg_read(.R0) & 0xFF);
                        try stdout.writeByte(ch8);
                    },
                    .GETC => {
                        const ch16 = @intCast(u16, try stdin.readIntNative(u8));
                        reg_write(.R0, ch16 & 0xFF);
                    },
                    .PUTS => {
                        const str = mem.spanZ(memory[reg_read(.R0)..:0]);
                        for (str) |ch16| {
                            try stdout.writeByte(try math.cast(u8, ch16 & 0xFF));
                        }
                    },
                    .PUTSP => {
                        const str = mem.spanZ(memory[reg_read(.R0)..:0]);
                        for (mem.sliceAsBytes(str)) |ch8| {
                            try stdout.writeByte(ch8);
                        }
                    },
                    .HALT => {
                        _ = try stdout.write("HALT");
                        running = false;
                    },
                }
            },
        }
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
