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

fn read_image(path: ?[]const u8) !void {
    if (path) |p| {
        return;
    }
    return error.NoPathProvided;
}

var memory = [_]u16{0} ** std.math.maxInt(u16);
var regs = [_]u16{0} ** @typeInfo(Register).Enum.fields.len;

inline fn set_reg(r: Register, val: u16) void {
    regs[@enumToInt(r)] = val;
}
inline fn get_reg(r: Register) u16 {
    return regs[@enumToInt(r)];
}

inline fn update_flags(r: Register) void {
    if (get_reg(r) == 0) {
        set_reg(.COND, @enumToInt(Flag.ZRO));
    } else if (get_reg(r) >> 15 != 0) {
        set_reg(.COND, @enumToInt(Flag.NEG));
    } else {
        set_reg(.COND, @enumToInt(Flag.POS));
    }
}

inline fn sign_extend(x: u16, bit_count: u4) u16 {
    var ret = x;
    if ((ret >> (bit_count - 1)) & 0x1 != 0) {
        ret |= (@as(u16, 0xFFFF) << bit_count);
    }
    return ret;
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
            .BR => {},
            .ADD => {
                const r0 = (instr >> 9) & 0x7;
                const r1 = (instr >> 6) & 0x7;
                const imm_flag = (instr >> 5) & 0x1;
                if (imm_flag == 1) {
                    const imm5 = sign_extend(instr & 0b11111, 5);
                    set_reg(.R0, get_reg(.R1) + imm5);
                } else {
                    const r2 = instr & 0x7;
                    set_reg(.R0, get_reg(.R1) + regs[r2]);
                }
                update_flags(.R0);
            },
            .LD => {},
            .ST => {},
            .JSR => {},
            .AND => {},
            .LDR => {},
            .STR => {},
            .NOT => {},
            .LDI => {},
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
    // TODO: the catch here isn't detecting that read_image has the return type of `!void`
    // so it gets mad that we try to catch it. Probably worth a bug report, if consistently reproducable.
    // for (args[1..]) |arg| {
    //     try read_image(arg) catch |e| { // TODO: why doesn't this work?
    //         std.debug.warn("Failed to load file: {}", .{arg});
    //         return e;
    //     };
    // }

    _ = try lc3();
}

test "smoke" {
    _ = try lc3();
}
