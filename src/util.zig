pub fn read_image(path: []const u8) !void {
    const fd = try fs.cwd().openFile(path, .{});
    try read_image_file(fd);
}

pub fn read_image_file(f: fs.File) !void {
    const file = f.reader();
    const origin = try file.readIntBig(u16);
    const max_read = math.maxInt(u16) - origin;
    var span = memory[origin..(origin + max_read)];
    _ = try file.readAll(mem.sliceAsBytes(span));
    for (span) |*word| {
        word.* = mem.bigToNative(u16, word.*);
    }
}

pub fn sign_extend(instr: u16, comptime sz: usize) i16 {
    var ret = instr & ~(@as(u16, 0xFFFF) << sz);
    if ((ret >> (sz - 1)) & 1 != 0) {
        ret |= (@as(u16, 0xFFFF) << sz);
    }
    return @bitCast(i16, ret);
}

