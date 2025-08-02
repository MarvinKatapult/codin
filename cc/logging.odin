package cc

import "core:fmt"
import "core:strings"

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
            fmt.printf("%s[cc] [D] ", BLUE);
        case .Proto:
            fmt.printf("%s[cc] [P] ", YELLOW);
    }
    for s in msg {
        fmt.print(s);
    }
    fmt.println(RESET);

}

log_error_with_token :: proc(token: Token, msg: ..string) {
    str_b := strings.builder_make();
    defer strings.builder_destroy(&str_b);
    strings.write_string(&str_b, "Token:");
    strings.write_string(&str_b, token.value);
    strings.write_string(&str_b, " - ");
    strings.write_string(&str_b, strings.join(msg, ""));
    strings.write_string(&str_b, " [");
    strings.write_uint(&str_b, uint(token.y));
    strings.write_string(&str_b, ":");
    strings.write_uint(&str_b, uint(token.x));
    strings.write_string(&str_b, "]");
    log(.Error, strings.to_string(str_b));
}

log_ast :: proc(root: AstNode, x_offset: int = 0) {
    if x_offset == 0 {
        log(.Proto, "Logging AST:");
    }
    i := 0;
    for i < x_offset {
        fmt.print(" ");
        i += 1;
    }
    fmt.printfln("%s[%s]%s", YELLOW, root.type, RESET);
    for child in root.childs {
        log_ast(child, x_offset + 4);
    }

    if x_offset == 0 {
        fmt.printfln("%s%s%s", YELLOW, "--------------------", RESET);
    }
}
