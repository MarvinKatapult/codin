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

@(private="package")
LogType :: enum {
    Error = 0,
    Debug,
    Proto,
}

@(private="package")
log :: proc(type: LogType, msg: ..string, cc_prefix: bool = true, place := #caller_location) {
	prefix_str := cc_prefix ? "[cc]" : "";
    switch type {
        case .Error:
            fmt.eprintf("%s%s%s%s (%s) ", RED, prefix_str, UNDERLINE, cc_prefix ? "[E]" : "", place);
        case .Debug:
            fmt.printf("%s%s%s", BLUE, prefix_str, cc_prefix ? "[D] " : "");
        case .Proto:
            fmt.printf("%s%s%s", YELLOW, prefix_str, cc_prefix ? "[P] " : "");
    }
    for s in msg {
        fmt.print(s);
    }
    fmt.println(RESET);

}

@(private="package")
log_error_with_token :: proc(token: Token, msg: ..string) {
    str_b := strings.builder_make();
    defer strings.builder_destroy(&str_b);
    strings.write_string(&str_b, "Token: \'");
    strings.write_string(&str_b, token.value);
    strings.write_string(&str_b, "\' - ");
    content := strings.join(msg, "");
    defer delete(content);
    strings.write_string(&str_b, content);
    strings.write_string(&str_b, " [");
    strings.write_uint(&str_b, uint(token.y));
    strings.write_string(&str_b, ":");
    strings.write_uint(&str_b, uint(token.x));
    strings.write_string(&str_b, "]");
    log(.Error, strings.to_string(str_b));
}

@(private="package")
log_ast :: proc(root: ^AstNode, x_offset: int = 0) {
    if x_offset == 0 {
        log(.Proto, "Logging AST:");
    }
    i := 0;
    for i < x_offset {
        fmt.print(" ");
        i += 1;
    }
    switch v in root.value {
        case AstFunction:
            fmt.printfln("%s[%s] %s %s %s %s", YELLOW, root.type, v.ret_type, v.identifier, v.params, RESET);
        case AstStatement:
            fmt.printfln("%s[%s] %s", YELLOW, root.type, RESET);
        case AstExpression:
            fmt.printfln("%s[%s] %s %s Parent: %s %s", YELLOW, root.type, v.value, v.operator, root.parent.type, RESET);
        case:
            fmt.printfln("%s[%s]%s", YELLOW, root.type, RESET);
    }
    for child in root.childs {
        log_ast(child, x_offset + 4);
    }

    if x_offset == 0 {
        fmt.printfln("%s%s%s", YELLOW, "--------------------", RESET);
    }
}
