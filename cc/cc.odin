package cc

import "core:fmt"
import "core:os"

start_compiling :: proc() {
	if len(os.args) < 2 {
		fmt.printfln("My C Compiler\nUsage:%s [*.c]", os.args[0]);
		return;
	}

	compile_file(os.args[1]);
}

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
    defer cleanup_ast_node(ast);
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

	src_file := "main.fasm";
    log(.Proto, "Writing Assembly to ", src_file, "!");
    os.write_entire_file(src_file, transmute([]u8)assembler);

    log(.Proto, assembler, cc_prefix = false);

	compile_asm(assembler, "main.fasm", "my_program") or_return;

    return true;
}
