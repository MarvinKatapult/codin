package cc

import "core:fmt"
import "core:os"

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

    assembler := generate_asm(ast);
    if assembler == "" {
        log(.Error, "Generating Assembly was not successful");
        return false;
    }
    defer delete(assembler);

    log(.Proto, "Writing Assembly to a.fasm!");
    os.write_entire_file("a.fasm", transmute([]u8)assembler);

    log(.Proto, assembler);

    return true;
}
