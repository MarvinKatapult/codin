package cc

import "core:strings"
import "core:fmt"
import "core:os/os2"
import "core:sys/linux/"

@(private="file")
append_fasm_header :: proc(str_b: ^strings.Builder) -> bool {
    strings.write_string(str_b,
        "format ELF64 executable 3\n"
    )
    strings.write_string(str_b,
        "entry start\n"
    )
    strings.write_string(str_b,
        "\n"
    )
    strings.write_string(str_b,
        "segment readable executable\n"
    )

    return true;
}

@(private="file")
generate_asm_for_operator :: proc(str_b: ^strings.Builder, expression_node: ^AstNode) -> bool {
	expression_t := expression_node.value.(AstExpression);

	switch expression_t.operator {
		case .OP_BIT_NEGATION:
			strings.write_string(str_b, "\tnot rax\t\t; negating Value in RAX (~)\n");
		case .OP_LOGICAL_NOT:
			strings.write_string(str_b, "\tcmp eax, 0\t; compare eax with 0\n");
			strings.write_string(str_b, "\tsete al\t\t; setting al to 1 if equal flag is set\n");
			strings.write_string(str_b, "\tmovzx eax, al\t; move al in eax\n");
		case .OP_UNARY_MINUS:
			strings.write_string(str_b, "\tneg rax\t\t; negating Value in RAX (~)\n");
		case .OP:
			log(.Error, "Not a valid Operator!");
			return false;
	}

	strings.write_string(str_b, "\n");

	return true;
}

@(private="file")
generate_asm_for_expression_bottom_up :: proc(str_b: ^strings.Builder, expression_node: ^AstNode) -> bool {
	expression_t := expression_node.value.(AstExpression);
	#partial switch expression_node.type {
		case .AST_EXPRESSION_CONSTANT:
			strings.write_string(str_b, "\tmov rax, ");
			strings.write_string(str_b, expression_t.value);
			strings.write_string(str_b, "\t; moving value of expression directly into rax\n\n");
			return true;
		case .AST_EXPRESSION_UNARY:
			return generate_asm_for_operator(str_b, expression_node);
	}

	log(.Error, "Expression must be constant or unary expression!");

	return false;
}

@(private="file")
calc_value_of_expression :: proc(str_b: ^strings.Builder, expression_node: ^AstNode) -> bool {
	tmp_expression_tree: [dynamic]^AstNode;
	defer delete(tmp_expression_tree);

	tmp_node := expression_node;
	for {
		if len(tmp_node.childs) > 0 {
			tmp_node = tmp_node.childs[0];
		} else do break;
	}

	for {
		if !generate_asm_for_expression_bottom_up(str_b, tmp_node) {
			log(.Error, "Generating asm for expression was not successful");
			return false;
		}
		tmp_node = tmp_node.parent;
		if tmp_node.type == .AST_RETURN_STATEMENT do break;
	}

	return true;
}

@(private="file")
generate_for_statement :: proc(str_b: ^strings.Builder, statement_node: ^AstNode, function_label: string) -> bool {

    statement_t := statement_node.value.(AstStatement);

    #partial switch statement_node.type {
    case .AST_RETURN_STATEMENT:
			if len(statement_node.childs) <= 0 do return false;
            ok := calc_value_of_expression(str_b, statement_node.childs[0]);
            if !ok do return false;

            if function_label != "start" {
                strings.write_string(str_b, "\t; Setting return code\n");
                strings.write_string(str_b, "\tret \t\t; Returning\n");
            } else {
                strings.write_string(str_b, "\tmov rdi, rax\t; move calculated return value in rdi\n");
                strings.write_string(str_b, "\tmov rax, 60\t; (sys_exit)\n");
                strings.write_string(str_b, "\tsyscall\t\t; Shutting down program\n");
            }
    }

    return true;
}

@(private="file")
generate_for_function :: proc(str_b: ^strings.Builder, function_node: ^AstNode) -> bool {

    function_t := function_node.value.(AstFunction);

    function_label := function_t.identifier;
    if function_label == "main" do function_label = "start";

    strings.write_string(str_b, function_label);
    strings.write_string(str_b, ":\n");

    for child in function_node.childs {
        if !generate_for_statement(str_b, child, function_label) do return false;
    }
    
    return true;
}

@(private="file")
generate_for_ast_node :: proc(str_b: ^strings.Builder, node: ^AstNode) -> bool {
    #partial switch node.type {
        case .AST_PROGRAM:
            for child in node.childs {
                if !generate_for_ast_node(str_b, child) do return false;
            }
        case .AST_FUNCTION:
            if !generate_for_function(str_b, node) do return false;
    }
    return true;
}

@(private="package")
generate_asm :: proc(ast: ^AstNode) -> string {
    str_b := strings.builder_make();
    defer strings.builder_destroy(&str_b);

    if !append_fasm_header(&str_b) do return "";
    if !generate_for_ast_node(&str_b, ast) do return "";

    return strings.clone(strings.to_string(str_b));
}

@(private="package")
compile_asm :: proc(asm_str: string, src_name: string, bin_name: string) -> bool {
	process_state, stdout, stderr, err := os2.process_exec(
		os2.Process_Desc { 
			command = {"fasm", src_name, bin_name},
		}, 
		context.temp_allocator
	);

	log(.Proto, "FASM Output:");
	if len(stdout) > 0 {
		yellow_stdout := fmt.tprintf("%s%s%s", YELLOW, transmute(string)stdout, RESET);
		log(.Proto, yellow_stdout, cc_prefix = false);
	}
	if len(stderr) > 0 {
		stderr_fmt := fmt.tprintf("FASM-%s", transmute(string)stderr);
		log(.Error, stderr_fmt, cc_prefix = false);
		return false;
	}

	if err != nil {
		log(.Error, "FASM could not be started!\nFasm is a dependency of this C compiler:\nhttps://flatassembler.net/");
		return false;
	} else {
		log(.Proto, "Compiling of file ", bin_name, " was successful!");
	}
	// r-xr-xr-x
	if os2.chmod(bin_name, 0o755) != nil {
		log(.Error, "File rights of ", bin_name, " could not be set properly!");
		return false;
	}

	return true;
}
