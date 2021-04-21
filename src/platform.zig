const builtin = @import("builtin");
const windows = @import("windows.zig");

pub const check_key = switch (builtin.os.tag) {
    .windows => windows.check_key,
    else => unreachable, // TODO: linux
};

pub const disable_input_buffering = switch (builtin.os.tag) {
    .windows => windows.disable_input_buffering,
    else => unreachable, // TODO: linux
};

pub const restore_input_buffering = switch (builtin.os.tag) {
    .windows => windows.restore_input_buffering,
    else => unreachable, // TODO: linux
};
