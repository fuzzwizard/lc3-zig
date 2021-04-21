const c = @cImport({
    @cInclude("Windows.h");
    @cInclude("conio.h"); // kbhit
});

var hStdin: c.HANDLE = c.INVALID_HANDLE_VALUE;

pub fn check_key() u16 {
    if (c.WaitForSingleObject(c.hStdin, 1000) == c.WAIT_OBJECT_0) {
        return c._kbhit();
    }
    return 0;
}

var fdwMode: c.DWORD;
var fdwOldMode: c.DWORD;

pub fn disable_input_buffering() void {
    hStdin = c.GetStdHandle(c.STD_INPUT_HANDLE);
    c.GetConsoleMode(hStdin, &fdwOldMode); // save old mode
    fdwMode = fdwOldMode ^ c.ENABLE_ECHO_INPUT // no input echo
        ^ c.ENABLE_LINE_INPUT; // return when one or more characters are available
    c.SetConsoleMode(hStdin, fdwMode); // set new mode
    c.FlushConsoleInputBuffer(hStdin); // clear buffer
}

pub fn restore_input_buffering() void {
    c.SetConsoleMode(hStdin, fdwOldMode);
}
