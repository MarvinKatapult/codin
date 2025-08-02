package cc

import "core:fmt"

compile_file :: proc(filepath: string) -> bool {
    log(.Debug)
    tokens := lex(filepath);
    if tokens == nil {
        log(.Error, "Lexing was not successful");
        return false;
    }

    cleanup_tokens(&tokens);

    return true;
}
