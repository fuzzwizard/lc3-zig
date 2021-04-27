const std = @import("std");
const win = std.os.windows;
const k32 = win.kernel32;
const c = @cImport({
    // @cInclude("windows.h");
    @cInclude("conio.h");
});

var hStdin: win.HANDLE = win.INVALID_HANDLE_VALUE;

pub fn check_key() u16 {
    // if (try win.WaitForSingleObject(hStdin, 1000) == win.WAIT_OBJECT_0) {
    // return c._kbhit();
    // }
    return 0;
}
var fdwMode: win.DWORD = undefined;
var fdwOldMode: win.DWORD = undefined;

pub fn disable_input_buffering() !void {
    hStdin = @as(win.HANDLE, try win.GetStdHandle(win.STD_INPUT_HANDLE));
    // _ = k32.GetConsoleMode(hStdin, &fdwOldMode); // save old mode
    // fdwMode = fdwOldMode ^ c.ENABLE_ECHO_INPUT // no input echo
    // ^ c.ENABLE_LINE_INPUT; // return when one or more characters are available
    // k32.SetConsoleMode(hStdin, fdwMode); // set new mode
    // win.FlushConsoleInputBuffer(hStdin); // clear buffer
}

pub fn restore_input_buffering() void {
    // k32.SetConsoleMode(hStdin, fdwOldMode);
}
