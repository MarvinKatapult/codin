package cc

import "core:fmt"
import "core:os"

CompileFlags :: struct {
	is_object: bool,
	output_file: string,
	input_file: string,
}
compile_flags: CompileFlags = {
	is_object = false,
	output_file = "a.out",
	input_file = "",
}

start_compiling :: proc() {
	if len(os.args) < 2 {
		fmt.printfln("Codin C-Compiler (WIP)")
		fmt.printfln("USAGE:  codin (OPTIONS) [*.c]")
		fmt.printfln("OPTIONS:")
		fmt.printfln("        -c          Compile file to .o (Object file) (DEF=FALSE)")
		fmt.printfln("        -o [OUTPUT] Overwrite output file            (DEF=a.out)")
		return
	}

	if !parse_args() {
		os.exit(1)
	}
	log(.Debug, fmt.tprint(compile_flags))

	if !compile_file() {
		os.exit(1)
	}
}

parse_args :: proc() -> bool {
	skip: bool
	for arg, i in os.args {
		if i == 0 do continue
		if skip {
			skip = false
			continue
		}

		if arg == "-c" {
			compile_flags.is_object = true
			continue
		}

		if arg == "-o" {
			if len(os.args) - 1 == i {
				log(.Error, "No output file specified after -o")
				return false
			}
			skip = true
			compile_flags.output_file = os.args[i+1]
			continue
		}
		
		if compile_flags.input_file != "" {
			log(.Error, "Too many input files specified!")
			return false
		}

		compile_flags.input_file = arg
	}

	return true
}

compile_file :: proc() -> bool {
	context.allocator = context.temp_allocator
	defer free_all(context.allocator)

	log(.Proto, "Compiling file: ", compile_flags.input_file)
	log(.Proto, "Lexing file: ", compile_flags.input_file)
	tokens := lex(compile_flags.input_file)
	if tokens == nil {
		log(.Error, "Lexing was not successful")
		return false
	}

	for token in tokens {
		log(.Proto, fmt.tprint(token))
	}

	log(.Proto, "Building AST for file: ", compile_flags.input_file)
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

	if !compile_asm(asm_file) do return false

	return true
}
