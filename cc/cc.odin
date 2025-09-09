package cc

import "core:fmt"
import "core:os"

start_compiling :: proc() {
	if len(os.args) < 2 {
		fmt.printfln("Codin C-Compiler (WIP)\nUsage:%s [*.c]", os.args[0])
		return
	}

	if !compile_file(os.args[1]) {
		os.exit(1)
	}
}

compile_file :: proc(filepath: string) -> bool {
	context.allocator = context.temp_allocator
	defer free_all(context.allocator)

	log(.Proto, "Compiling file: ", filepath)
	log(.Proto, "Lexing file: ", filepath)
	tokens := lex(filepath)
	if tokens == nil {
		log(.Error, "Lexing was not successful")
		return false
	}

	for token in tokens {
		log(.Proto, fmt.tprint(token))
	}

	log(.Proto, "Building AST for file: ", filepath)
	ok, ast := build_ast(tokens[:])
	log_ast(ast)
	if !ok {
		log(.Error, "Building AST was not successful")
		return false
	}

	assembler := generate_asm(ast)
	if assembler == "" {
		log(.Error, "Generating Assembly was not successful")
		return false
	}

	output_dir :: "./output"
	if !os.is_dir(output_dir) {
		if os.make_directory(output_dir, 0o775) != nil {
			log(.Error, "Unable to create Output directory!")
			return false
		}
	}

	asm_file :: "./output/main.fasm"
	log(.Proto, "Writing Assembly to ", asm_file, "!")
	if !os.write_entire_file(asm_file, transmute([]u8)assembler) do return false

	log(.Proto, assembler, cc_prefix = false)

	bin_name :: "./output/main"
	if !compile_asm(asm_file, bin_name) do return false

	return true
}
