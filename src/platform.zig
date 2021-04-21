const builtin = @import("builtin");
const windows = @import("windows.zig");

pub const platform = switch (builtin.os.tag) {
    .windows => windows,
    else => unreachable,
};

pub const check_key = platform.check_key;
pub const disable_input_buffering = platform.disable_input_buffering;
pub const restore_input_buffering = platform.restore_input_buffering;
