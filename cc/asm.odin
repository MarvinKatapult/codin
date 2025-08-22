package cc

import "core:strings"
import "core:fmt"
import "core:os/os2"
import "core:sys/linux/"

INT_BYTE_SIZE :: 8

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

	return true
}

@(private="file")
generate_asm_for_operator :: proc(str_b: ^strings.Builder, expression_node: ^AstNode) -> bool {
	expression_t := expression_node.value.(AstExpression)

	switch expression_t.operator {
		case .OP_BIT_NEGATION:
			strings.write_string(str_b, "\tnot rax\t\t; negating Value in RAX (~)\n")
		case .OP_LOGICAL_NOT:
			strings.write_string(str_b, "\txor rax, 1\t; Flip LSB true <-> false\n")
		case .OP_UNARY_MINUS:
			strings.write_string(str_b, "\tneg rax\t\t; negating Value in RAX (~)\n")
		case .OP_BINARY_PLUS:
			strings.write_string(str_b, "\tadd rax, rdi\t; Adding rax and rdi\n")
		case .OP_BINARY_MINUS:
			strings.write_string(str_b, "\tsub rdi, rax\t; Subtracting rdi from rax\n")
			strings.write_string(str_b, "\tmov rax, rdi\t; Moving result in rax\n")
		case .OP_BINARY_MULT:
			strings.write_string(str_b, "\tmul rdi\t\t; Multiplying rdi with rax\n")
		case .OP_BINARY_DIV:
			strings.write_string(str_b, "\tmov rdx, rdi	; Moving rdi to rdx\n")
			strings.write_string(str_b, "\tmov rcx, rax	; Moving rax to rcx\n")
			strings.write_string(str_b, "\tmov rax, rdx	; moving rdx in rax\n")
			strings.write_string(str_b, "\tcqo\t\t; Expand RAX to RAX:RDX 128 Bit Register\n")
			strings.write_string(str_b, "\tidiv rcx\t; RAX = RAX / RCX\n")
		case .OP_BINARY_LESS:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax < rdi\n")
			strings.write_string(str_b, "\tsetl al\t\t; Set al to 1 if true\n")
		case .OP_BINARY_LESS_EQUAL:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax <= rdi\n")
			strings.write_string(str_b, "\tsetle al\t\t; Set al to 1 if true\n")
		case .OP_BINARY_GREATER:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax > rdi\n")
			strings.write_string(str_b, "\tsetg al\t\t; Set al to 1 if true\n")
		case .OP_BINARY_GREATER_EQUAL:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax >= rdi\n")
			strings.write_string(str_b, "\tsetge al\t\t; Set al to 1 if true\n")
		case .OP_BINARY_EQUAL:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax == rdi\n")
			strings.write_string(str_b, "\tsete al\t\t; Set al to 1 if true\n")
		case .OP_BINARY_NOT_EQUAL:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax != rdi\n")
			strings.write_string(str_b, "\tsetne al\t; Set al to 1 if true\n")
		case .OP:
			log(.Error, "Not a valid Operator:", fmt.tprintf("%s", expression_node^))
			return false
	}

	strings.write_string(str_b, "\n")

	return true
}

@(private="file")
generate_asm_for_expr :: proc(str_b: ^strings.Builder, expression_node: ^AstNode, scope: ^map[string]string) -> bool {
	expression_t := expression_node.value.(AstExpression)
	log(.Debug, "Calling generate_asm_for_expr with Expressiontype:", fmt.tprintf("%s", expression_node.type))
	#partial switch expression_node.type {
		case .AST_EXPRESSION_VARIABLE:
			if len(expression_node.childs) != 0 {
				log(.Error, "Constant Expression Type cannot have childs")
				return false
			}
			strings.write_string(str_b, "\tmov qword rax, ")
			strings.write_string(str_b, scope[expression_t.value])
			strings.write_string(str_b, " ; moving value of variable directly into rax\n\n")
			return true
		case .AST_EXPRESSION_CONSTANT:
			if len(expression_node.childs) != 0 {
				log(.Error, "Constant Expression Type cannot have childs")
				return false
			}
			strings.write_string(str_b, "\tmov rax, ")
			strings.write_string(str_b, expression_t.value)
			strings.write_string(str_b, "\t; moving value of expression directly into rax\n\n")
			return true
		case .AST_EXPRESSION_UNARY:
			if len(expression_node.childs) != 1 {
				log(.Error, "Unary Expression Type must have exactly 1 child")
				return false
			}
			child := expression_node.childs[0]
			if !generate_asm_for_expr(str_b, child, scope) do return false

			return generate_asm_for_operator(str_b, expression_node)
			
		case .AST_EXPRESSION_BINARY:
			if len(expression_node.childs) != 2 {
				log(.Error, "Binary Expression Type must have exactly 2 childs")
				return false
			}

			valuel := expression_node.childs[0]
			if !generate_asm_for_expr(str_b, valuel, scope) do return false
		
			strings.write_string(str_b, "\tpush rax\t; Push to stack\n\n")

			valuer := expression_node.childs[1]
			if !generate_asm_for_expr(str_b, valuer, scope) do return false

			strings.write_string(str_b, "\tpop rdi\t\t; Popping Value to rdi off stack\n\n")

			return generate_asm_for_operator(str_b, expression_node)
	}

	log(.Debug, "Partially generated asm:\n", strings.to_string(str_b^))
	log(.Error, "Expression must be constant or unary expression!")

	return false
}

@(private="file")
calc_value_of_expression :: proc(str_b: ^strings.Builder, expression_node: ^AstNode, scope: ^map[string]string) -> bool {

	if !generate_asm_for_expr(str_b, expression_node, scope) {
		log(.Error, "Generating asm for expression was not successful")
		return false
	}

	return true
}

@(private="file")
generate_for_statement :: proc(str_b: ^strings.Builder, statement_node: ^AstNode, 
							   function_label: string, scope: ^map[string]string, rbp_offset: ^int) -> bool {

	statement_t := statement_node.value.(AstStatement)

	#partial switch statement_node.type {
		case .AST_RETURN_STATEMENT: fallthrough
		case .AST_EXPR_STATEMENT:
			if len(statement_node.childs) <= 0 do return false
			ok := calc_value_of_expression(str_b, statement_node.childs[0], scope)
			if !ok do return false

			if statement_node.type == .AST_RETURN_STATEMENT {
				strings.write_string(str_b, "\tleave\t\t; Restore old BasePointer and Free Stack memory\n")
				if function_label != "start" {
					strings.write_string(str_b, "\tret \t\t; Returning\n")
				} else {
					strings.write_string(str_b, "\tmov rdi, rax\t; move calculated return value in rdi\n")
					strings.write_string(str_b, "\tmov rax, 60\t; (sys_exit)\n")
					strings.write_string(str_b, "\tsyscall\t\t; Shutting down program\n")
				}
			}
		case .AST_VAR_DECLARE:
			// Declare things
			strings.write_string(str_b, "\tsub rsp, ")
			strings.write_int(str_b,	INT_BYTE_SIZE)
			strings.write_string(str_b, "\t; Allocate memory on the stack\n\n")
			statement_t := statement_node.value.(AstStatement)
			rbp_offset^ = rbp_offset^ - INT_BYTE_SIZE
			scope[statement_t.identifier] = strings.clone(fmt.tprintf("[rbp%d]", rbp_offset^))
			if len(statement_node.childs) > 0 {
				generate_for_statement(str_b, statement_node.childs[0], function_label, scope, rbp_offset)
			}
		case .AST_VAR_ASSIGNMENT:
			if len(statement_node.childs) <= 0 do return false
			ok := calc_value_of_expression(str_b, statement_node.childs[0], scope)
			if !ok do return false

			statement_t := statement_node.value.(AstStatement)
			strings.write_string(str_b, "\tmov qword ")
			strings.write_string(str_b, scope[statement_t.identifier])
			strings.write_string(str_b, ", ") 
			strings.write_string(str_b, "rax ; mov calculated value into variable: ")
			strings.write_string(str_b, statement_t.identifier)
			strings.write_string(str_b, "\n\n")
	}

	return true
}

delete_scope :: proc(s: ^map[string]string) {
	for str in s {
		delete(s[str])
	}
	delete(s^)
}

@(private="file")
generate_for_function :: proc(str_b: ^strings.Builder, function_node: ^AstNode) -> bool {

	function_t := function_node.value.(AstFunction)

	function_label := function_t.identifier
	if function_label == "main" do function_label = "start"

	strings.write_string(str_b, function_label)
	strings.write_string(str_b, ":\n")

	strings.write_string(str_b, "\tpush rbp\t; Save old base pointer\n")
	strings.write_string(str_b, "\tmov rbp, rsp\t; Set new Base pointer\n\n")

	scope: map[string]string
	defer delete_scope(&scope)

	rbp_offset: int = 0
	for child in function_node.childs {
		if !generate_for_statement(str_b, child, function_label, &scope, &rbp_offset) do return false
	}
	
	return true
}

@(private="file")
generate_for_ast_node :: proc(str_b: ^strings.Builder, node: ^AstNode) -> bool {
	#partial switch node.type {
		case .AST_PROGRAM:
			for child in node.childs {
				if !generate_for_ast_node(str_b, child) do return false
			}
		case .AST_FUNCTION:
			if !generate_for_function(str_b, node) do return false
	}
	return true
}

@(private="package")
generate_asm :: proc(ast: ^AstNode) -> string {
	str_b := strings.builder_make()
	defer strings.builder_destroy(&str_b)

	if !append_fasm_header(&str_b) do return ""
	if !generate_for_ast_node(&str_b, ast) do return ""

	return strings.clone(strings.to_string(str_b))
}

@(private="package")
compile_asm :: proc(asm_str: string, src_name: string, bin_name: string) -> bool {
	process_state, stdout, stderr, err := os2.process_exec(
		os2.Process_Desc { 
			command = {"fasm", src_name, bin_name},
		}, 
		context.temp_allocator
	)

	log(.Proto, "FASM Output:")
	if len(stdout) > 0 {
		yellow_stdout := fmt.tprintf("%s%s%s", YELLOW, transmute(string)stdout, RESET)
		log(.Proto, yellow_stdout, cc_prefix = false)
	}
	if len(stderr) > 0 {
		stderr_fmt := fmt.tprintf("FASM-%s", transmute(string)stderr)
		log(.Error, stderr_fmt, cc_prefix = false)
		return false
	}

	if err != nil {
		log(.Error, "FASM could not be started!\nFasm is a dependency of this C compiler:\nhttps://flatassembler.net/")
		return false
	} else {
		log(.Proto, "Compiling of file ", bin_name, " was successful!")
	}
	// r-xr-xr-x
	if os2.chmod(bin_name, 0o755) != nil {
		log(.Error, "File rights of ", bin_name, " could not be set properly!")
		return false
	}

	return true
}
