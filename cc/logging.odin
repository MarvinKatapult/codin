package cc

import "core:fmt"

@(private="package")
RED     :: "\033[31m"
@(private="package")
GREEN   :: "\033[32m"
@(private="package")
YELLOW  :: "\033[33m"
@(private="package")
BLUE    :: "\033[34m"
@(private="package")
MAGENTA :: "\033[35m"
@(private="package")
CYAN    :: "\033[36m"
@(private="package")
WHITE   :: "\033[37m"
@(private="package")
RESET   :: "\033[0m"

// Styles
@(private="package")
BOLD      :: "\033[1m"
@(private="package")
UNDERLINE :: "\033[4m"

LogType :: enum {
    Error = 0,
    Debug,
    Proto,
}

@(private="package")
log :: proc(type: LogType, msg: ..string) {
    switch type {
        case .Error:
            fmt.eprintf("%s%s[cc] [E] ", RED, UNDERLINE);
        case .Debug:
            fmt.printf("%s[cc] [D] ", YELLOW);
        case .Proto:
            fmt.printf("%s[cc] [P] ", BLUE);
    }
    for s in msg {
        fmt.print(s);
    }
    fmt.println(RESET);

}
