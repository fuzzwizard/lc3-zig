const std = @import("std");
const expect = std.testing.expect;
const util = @import("util.zig");

const sign_extend = util.sign_extend;

fn old_sign_extend(x: u16, bit_count: u4) i16 {
    var ret = x;
    if ((ret >> (bit_count - 1)) & 1 != 0) {
        ret |= @as(u16, 0xFFFF) << bit_count;
    }
    return @bitCast(i16, ret);
}

test "sign_extend" {
    //            .ADD sr0 dr1 f imm5
    const add = 0b0001_000_000_1_11111;
    expect(sign_extend(add, 5) == -1);
    expect(sign_extend(add, 5) == old_sign_extend(add & 0x1F, 5));
    //            .ADD sr0 dr1 f imm5
    const add2 = 0b0001_000_000_1_00001;
    expect(sign_extend(add2, 5) == 1);
    expect(sign_extend(add2, 5) == old_sign_extend(add2 & 0x1F, 5));
    //            .STR sr0 dr1 imm6
    const str = 0b0111_000_000_111111;
    expect(sign_extend(str, 6) == -1);
    expect(sign_extend(str, 6) == old_sign_extend(str & 0x3F, 6));
    //            .LDI dr0 PCOffset9
    const ldi = 0b1010_000_111111111;
    expect(sign_extend(ldi, 9) == -1);
    expect(sign_extend(ldi, 9) == old_sign_extend(ldi & 0x1FF, 9));
    //            .JSR f PCOffset11
    const jsr = 0b0100_1_11111111111;
    expect(sign_extend(jsr, 11) == -1);
    expect(sign_extend(jsr, 11) == old_sign_extend(jsr & 0x7FF, 11));
}
