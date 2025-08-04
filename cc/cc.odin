package cc

import "core:fmt"

compile_file :: proc(filepath: string) -> bool {
    log(.Proto, "Compiling file: ", filepath);
    log(.Proto, "Lexing file: ", filepath);
    tokens := lex(filepath);
    if tokens == nil {
        log(.Error, "Lexing was not successful");
        return false;
    }
    defer cleanup_tokens(&tokens);

    for token in tokens {
        log(.Proto, fmt.tprint(token));
    }

    log(.Proto, "Building AST for file: ", filepath);
    ok, ast := build_ast(tokens[:]);
    log_ast(ast);
    defer cleanup_ast_node(&ast);
    if !ok {
        log(.Error, "Building AST was not successful");
        return false;
    }

    return true;
}
