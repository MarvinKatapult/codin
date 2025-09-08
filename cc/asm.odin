package cc

import "core:strings"
import "core:fmt"
import "core:os/os2"
import "core:sys/linux/"

@(private="file")
INT_BYTE_SIZE :: 8

@(private="file")
FunctionInfo :: struct {
	identifier:  string,
	return_type: AstDataType,
	params:      [dynamic]AstDataType,
}

@(private="file")
FileInfo :: struct {
	callables: map[string]FunctionInfo
}

@(private="file")
FunctionScope :: struct {
	variables:   map[string]string,
	label:       string,
	label_count: ^int,
	rbp_offset:  ^int,
	break_label: string,
}

@(private="file")
append_fasm_header :: proc(str_b: ^strings.Builder) {
	strings.write_string(str_b, "format ELF64 executable 3\n")
	strings.write_string(str_b, "entry start\n")
	strings.write_string(str_b, "\n")
	strings.write_string(str_b, "segment readable executable\n")
}

@(private="file")
write_label :: proc(str_b: ^strings.Builder, func_scope: ^FunctionScope, advance := false) -> (ret: int) {
	strings.write_string(str_b, ".L")
	ret = func_scope.label_count^
	strings.write_int(str_b, ret)
	if advance do func_scope.label_count^ = func_scope.label_count^ + 1
	return ret
}

@(private="file")
generate_asm_for_if :: proc(str_b: ^strings.Builder, if_node: ^AstNode,
							func_scope: ^FunctionScope, file_info: ^FileInfo) -> bool {
	assert(len(if_node.childs) >= 2)

	// Checking condition
	if !generate_asm_for_expr(str_b, if_node.childs[0], func_scope, file_info) do return false
	strings.write_string(str_b, "\tcmp rax, 0\t; IF Check if condition is false\n")
	strings.write_string(str_b, "\tje ")
	potential_else := write_label(str_b, func_scope, advance = true)
	strings.write_string(str_b, "\t\t; Jump over scope if condition is false\n")

	// Scope
	generate_asm_for_scope(str_b, if_node.childs[1], func_scope, file_info) or_return

	strings.write_string(str_b, "\tjmp ")
	end_of_if_scope := write_label(str_b, func_scope, advance = true)
	strings.write_string(str_b, "\t\t; Jump to end of if-statement\n\n")

	strings.write_string(str_b, fmt.tprintf("\t.L%d:\t\t; Potential else-if or else\n\n", potential_else))

	for else_or_else_if, i in if_node.childs {
		if i <= 1 do continue // Skip condition and scope node
		// Here are nodes such as else and else if
		if else_or_else_if.type == .AST_ELSE_IF {
			if !generate_asm_for_expr(str_b, else_or_else_if.childs[0], func_scope, file_info) do return false
			strings.write_string(str_b, "\tcmp rax, 0\t; ELSE-IF Check if condition is false\n")
			strings.write_string(str_b, "\tje ")
			potential_else := write_label(str_b, func_scope, advance = true)
			strings.write_string(str_b, "\t\t; Jump over scope if condition is false\n")

			generate_asm_for_scope(str_b, else_or_else_if.childs[1], func_scope, file_info) or_return

			strings.write_string(str_b, "\tjmp .L")
			strings.write_int(str_b, end_of_if_scope)
			strings.write_string(str_b, "\t\t; Jump to end of if-statement\n\n")

			strings.write_string(str_b, fmt.tprintf("\t.L%d:\t\t; Potential else-if or else\n\n", potential_else))
		}

		if else_or_else_if.type == .AST_ELSE {
			generate_asm_for_scope(str_b, else_or_else_if.childs[0], func_scope, file_info) or_return
		}
	}

	strings.write_string(str_b, fmt.tprintf("\t.L%d:\t\t; If End\n\n", end_of_if_scope))

	return true
}

@(private="file")
generate_asm_for_while :: proc(str_b: ^strings.Builder, statement_node: ^AstNode,
							   func_scope: ^FunctionScope, file_info: ^FileInfo) -> bool {
	assert(len(statement_node.childs) == 2)

	condition_node := statement_node.childs[0]
	scope_node     := statement_node.childs[1]

	strings.write_string(str_b, "\t")
	before_condition := write_label(str_b, func_scope, advance = true)
	strings.write_string(str_b, ":\t; while condition\n\n")

	generate_asm_for_expr(str_b, condition_node, func_scope, file_info) or_return

	strings.write_string(str_b, "\tcmp rax, 0\t; Check if condition is false\n")
	strings.write_string(str_b, "\tje ")
	after_while := write_label(str_b, func_scope, advance = true)
	strings.write_string(str_b, "\t\t; Jump to end of while\n\n")

	old_label := func_scope.break_label
	func_scope.break_label = strings.clone(fmt.tprintf(".L%d", after_while))

	generate_asm_for_scope(str_b, scope_node, func_scope, file_info) or_return

	func_scope.break_label = old_label

	strings.write_string(str_b, fmt.tprintf("\tjmp .L%d\t\t; Jump to begin of while\n", before_condition))
	strings.write_string(str_b, fmt.tprintf("\t.L%d:\t\t; End of while\n\n", after_while))

	return true
}

@(private="file")
generate_asm_for_for :: proc(str_b: ^strings.Builder, statement_node: ^AstNode, 
							 func_scope: ^FunctionScope, file_info: ^FileInfo) -> bool {

	begin_node := statement_node.childs[0]
	condition  := statement_node.childs[1]
	iteration  := statement_node.childs[2]
	scope      := statement_node.childs[3]

	old_variables := func_scope.variables
	func_scope.variables = clone_variables(func_scope.variables)^

	if begin_node.type != .AST_NOTHING {
		generate_asm_for_statement(str_b, begin_node, func_scope, file_info) or_return
	}

	strings.write_string(str_b, "\t")
	before_condition := write_label(str_b, func_scope, advance = true)
	strings.write_string(str_b, ":\t; for condition\n\n")

	if condition.type != .AST_NOTHING {
		generate_asm_for_expr(str_b, condition, func_scope, file_info) or_return

		strings.write_string(str_b, "\tcmp rax, 0\t; Check if condition is false\n")
		strings.write_string(str_b, "\tje ")
		write_label(str_b, func_scope)
		strings.write_string(str_b, "\t\t; Jump to end of for\n\n")
	}
	after_for := func_scope.label_count^
	func_scope.label_count^ = func_scope.label_count^ + 1

	old_label := func_scope.break_label
	func_scope.break_label = strings.clone(fmt.tprintf(".L%d", after_for))

	generate_asm_for_scope(str_b, scope, func_scope, file_info) or_return

	func_scope.break_label = old_label

	if iteration.type != .AST_NOTHING {
		generate_asm_for_statement(str_b, iteration, func_scope, file_info) or_return
	}

	strings.write_string(str_b, fmt.tprintf("\tjmp .L%d\t\t; Jump to begin of for\n", before_condition))

	strings.write_string(str_b, fmt.tprintf("\t.L%d:\t\t; End of for\n\n", after_for))

	func_scope.variables = old_variables

	return true
}

@(private="file")
generate_asm_for_operator :: proc(str_b: ^strings.Builder, expression_node: ^AstNode, func_scope: ^FunctionScope) -> bool {

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
		case .OP_BINARY_MOD:
			strings.write_string(str_b, "\tmov rdx, rdi	; Moving rdi to rdx\n")
			strings.write_string(str_b, "\tmov rcx, rax	; Moving rax to rcx\n")
			strings.write_string(str_b, "\tmov rax, rdx	; moving rdx in rax\n")
			strings.write_string(str_b, "\tcqo\t\t; Expand RAX to RAX:RDX 128 Bit Register\n")
			strings.write_string(str_b, "\tidiv rcx\t; RAX = RAX / RCX\n")
			strings.write_string(str_b, "\tmov rax, rdx\t; Move remainder into rax\n")
		case .OP_BINARY_LESS:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax < rdi\n")
			strings.write_string(str_b, "\tsetl al\t\t; Set al to 1 if true\n")
			strings.write_string(str_b, "\tmovsx rax, al\t; Extend al to rax\n")
		case .OP_BINARY_LESS_EQUAL:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax <= rdi\n")
			strings.write_string(str_b, "\tsetle al\t\t; Set al to 1 if true\n")
			strings.write_string(str_b, "\tmovsx rax, al\t; Extend al to rax\n")
		case .OP_BINARY_GREATER:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax > rdi\n")
			strings.write_string(str_b, "\tsetg al\t\t; Set al to 1 if true\n")
			strings.write_string(str_b, "\tmovsx rax, al\t; Extend al to rax\n")
		case .OP_BINARY_GREATER_EQUAL:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax >= rdi\n")
			strings.write_string(str_b, "\tsetge al\t\t; Set al to 1 if true\n")
			strings.write_string(str_b, "\tmovsx rax, al\t; Extend al to rax\n")
		case .OP_BINARY_EQUAL:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax == rdi\n")
			strings.write_string(str_b, "\tsete al\t\t; Set al to 1 if true\n")
			strings.write_string(str_b, "\tmovsx rax, al\t; Extend al to rax\n")
		case .OP_BINARY_NOT_EQUAL:
			strings.write_string(str_b, "\tcmp rdi, rax\t; Compare rax != rdi\n")
			strings.write_string(str_b, "\tsetne al\t; Set al to 1 if true\n")
				strings.write_string(str_b, "\tmovsx rax, al\t; Extend al to rax\n")
		case .OP_LOGICAL_OR:
			strings.write_string(str_b, "\ttest rdi, rdi\t; Check if rdi != 0\n")
			strings.write_string(str_b, "\tsetnz dl\t; dl = (rdi != 0) ? 1 : 0\n")
			strings.write_string(str_b, "\tjnz ")
			write_label(str_b, func_scope)
			strings.write_string(str_b, "\t\t; Short Circuit Evaluation\n")
			strings.write_string(str_b, "\ttest rax, rax\t; Check if rax != 0\n")
			strings.write_string(str_b, "\tsetnz al \t; al = 1 if 0F == 1 else = 0\n")
			strings.write_string(str_b, "\tor dl, al\t; Logical OR\n\t")
			write_label(str_b, func_scope, advance = true)
			strings.write_string(str_b, ":\n")
			strings.write_string(str_b, "\tmovzx rax, dl\t; -> 64-Bit\n")
		case .OP_LOGICAL_AND:
			strings.write_string(str_b, "\ttest rdi, rdi\t; Check if rdi != 0\n")
			strings.write_string(str_b, "\tsetnz dl\t; dl = (rdi != 0) ? 1 : 0\n")
			strings.write_string(str_b, "\tjz ")
			write_label(str_b, func_scope)
			strings.write_string(str_b, "\t\t; Short Circuit Evaluation\n")
			strings.write_string(str_b, "\ttest rax, rax\t; Check if rax != 0\n")
			strings.write_string(str_b, "\tsetnz al \t; al = 1 if 0F == 1 else = 0\n")
			strings.write_string(str_b, "\tand dl, al\t; Logical AND\n\t")
			write_label(str_b, func_scope, advance = true)
			strings.write_string(str_b, ":\n")
			strings.write_string(str_b, "\tmovzx rax, dl\t; -> 64-Bit\n")
		case .OP_BIT_OR:
			strings.write_string(str_b, "\tor rax, rdi\t; Bit OR\n")
		case .OP_BIT_AND:
			strings.write_string(str_b, "\tand rax, rdi\t; Bit AND\n")
		case .OP_BIT_XOR:
			strings.write_string(str_b, "\txor rax, rdi\t; Bit XOR\n")
		case .OP_BIT_SHL:
			strings.write_string(str_b, "\tmov rcx, rax\t; Only cl can be used for shift operations\n")
			strings.write_string(str_b, "\tsal rdi, cl\t; rax << rdi\n")
			strings.write_string(str_b, "\tmov rax, rdi\t\n")
		case .OP_BIT_SHR:
			strings.write_string(str_b, "\tmov rcx, rax\t; Only cl can be used for shift operations\n")
			strings.write_string(str_b, "\tsar rdi, cl\t; rax >> rdi\n")
			strings.write_string(str_b, "\tmov rax, rdi\t\n")
			
		case .OP:
			log(.Error, "Not a valid Operator:", fmt.tprintf("%s", expression_node^))
			return false
	}

	strings.write_string(str_b, "\n")

	return true
}

@(private="file")
generate_asm_for_expr :: proc(str_b: ^strings.Builder, expression_node: ^AstNode, 
							  func_scope: ^FunctionScope, file_info: ^FileInfo) -> bool {
	expression_t := expression_node.value.(AstExpression)
	#partial switch expression_node.type {
		case .AST_EXPR_VARIABLE:
			if len(expression_node.childs) != 0 {
				log(.Error, "Constant Expression Type cannot have childs")
				return false
			}
			strings.write_string(str_b, "\tmov qword rax, ")
			if expression_t.value not_in func_scope.variables {
				log(.Error, "Variable ", expression_t.value, " not declared!")
				return false
			}
			strings.write_string(str_b, func_scope.variables[expression_t.value])
			strings.write_string(str_b, " ; moving value of variable directly into rax\n\n")
			return true
		case .AST_EXPR_CONSTANT:
			if len(expression_node.childs) != 0 {
				log(.Error, "Constant Expression Type cannot have childs")
				return false
			}
			strings.write_string(str_b, "\tmov rax, ")
			strings.write_string(str_b, expression_t.value)
			strings.write_string(str_b, "\t; moving value of expression directly into rax\n\n")
			return true
		case .AST_EXPR_UNARY:
			if len(expression_node.childs) != 1 {
				log(.Error, "Unary Expression Type must have exactly 1 child")
				return false
			}
			child := expression_node.childs[0]
			if !generate_asm_for_expr(str_b, child, func_scope, file_info) do return false

			return generate_asm_for_operator(str_b, expression_node, func_scope)
			
		case .AST_EXPR_BINARY:
			if len(expression_node.childs) != 2 {
				log(.Error, "Binary Expression Type must have exactly 2 childs")
				return false
			}

			valuel := expression_node.childs[0]
			if !generate_asm_for_expr(str_b, valuel, func_scope, file_info) do return false
		
			strings.write_string(str_b, "\tpush rax\t; Push to stack\n\n")

			valuer := expression_node.childs[1]
			if !generate_asm_for_expr(str_b, valuer, func_scope, file_info) do return false

			strings.write_string(str_b, "\tpop rdi\t\t; Popping Value to rdi off stack\n\n")

			return generate_asm_for_operator(str_b, expression_node, func_scope)
		case .AST_FUNC_CALL:

			expression_t := expression_node.value.(AstExpression)
			func_identifier := expression_t.value
			
			if func_identifier not_in file_info.callables {
				log(.Error, fmt.tprintf("Function %s not found", func_identifier))
				return false
			}

			func_data := file_info.callables[func_identifier]

			if len(func_data.params) != len(expression_node.childs) {
				log(.Error, fmt.tprintf("Call to function %s mismatching parameter count", func_identifier))
				return false
			}

			// Put parameters for call on stack
			original_rbp_offset := func_scope.rbp_offset^
			for child in expression_node.childs {
				generate_asm_for_expr(str_b, child, func_scope, file_info) or_return

				strings.write_string(str_b, "\tpush rax\t; Allocate memory on the stack for parameter\n")

				rbp_offset := func_scope.rbp_offset
				rbp_offset^ = rbp_offset^ - INT_BYTE_SIZE
			}
			
			strings.write_string(str_b, "\tcall ")
			strings.write_string(str_b, expression_t.value)
			strings.write_string(str_b, "\t; Call Function\n\n")

			// Reset Memory from parameters on stack
			mem_to_free_from_stack := abs(func_scope.rbp_offset^ - original_rbp_offset)
			if mem_to_free_from_stack > 0 {
				strings.write_string(str_b, "\tadd rsp, ")
				strings.write_int(str_b, mem_to_free_from_stack)
				strings.write_string(str_b, "\t; Resetting Stack for func call\n\n")

				func_scope.rbp_offset^ += mem_to_free_from_stack
			}

			return true
	}

	log(.Error, "Expression must be constant or unary expression!")

	return false
}

@(private="package")
generate_asm_for_var_declare :: proc(str_b: ^strings.Builder, statement_node: ^AstNode, 
									 func_scope: ^FunctionScope, file_info: ^FileInfo) -> bool {

	strings.write_string(str_b, "\tsub rsp, ")
	strings.write_int(str_b,	INT_BYTE_SIZE)
	strings.write_string(str_b, "\t; Allocate memory on the stack\n\n")

	statement_t := statement_node.value.(AstStatement)
	if statement_t.identifier in func_scope.variables {
		log(.Error, "Variale redefinition!")
		return false
	}

	rbp_offset := func_scope.rbp_offset
	rbp_offset^ = rbp_offset^ - INT_BYTE_SIZE
	func_scope.variables[statement_t.identifier] = strings.clone(fmt.tprintf("[rbp%d]", rbp_offset^))
	if len(statement_node.childs) > 0 {
		if !generate_asm_for_statement(str_b, statement_node.childs[0], func_scope, file_info) {
			return false
		}
	}
	return true
}

@(private="file")
generate_asm_for_statement :: proc(str_b: ^strings.Builder, statement_node: ^AstNode, 
								   func_scope: ^FunctionScope, file_info: ^FileInfo) -> bool {
	statement_t := statement_node.value.(AstStatement)

	#partial switch statement_node.type {
		case .AST_RETURN_STATEMENT: fallthrough
		case .AST_EXPR_STATEMENT:
			if len(statement_node.childs) <= 0 do return false
			ok := generate_asm_for_expr(str_b, statement_node.childs[0], func_scope, file_info)
			if !ok do return false

			if statement_node.type == .AST_RETURN_STATEMENT {
				strings.write_string(str_b, "\tleave\t\t; Restore old BasePointer and Free Stack memory\n")
				if func_scope.label != "start" {
					strings.write_string(str_b, "\tret \t\t; Returning\n\n")
				} else {
					strings.write_string(str_b, "\tmov rdi, rax\t; move calculated return value in rdi\n")
					strings.write_string(str_b, "\tmov rax, 60\t; (sys_exit)\n")
					strings.write_string(str_b, "\tsyscall\t\t; Shutting down program\n")
				}
			}
		case .AST_VAR_DECLARE:
			generate_asm_for_var_declare(str_b, statement_node, func_scope, file_info) or_return
		case .AST_VAR_ASSIGNMENT:
			if len(statement_node.childs) <= 0 do return false
			ok := generate_asm_for_expr(str_b, statement_node.childs[0], func_scope, file_info)
			if !ok do return false

			statement_t := statement_node.value.(AstStatement)
			strings.write_string(str_b, "\tmov qword ")
			if statement_t.identifier not_in func_scope.variables {
				log(.Error, "Variable ", statement_t.identifier, " not declared!")
				return false
			}
			strings.write_string(str_b, func_scope.variables[statement_t.identifier])
			strings.write_string(str_b, ", ") 
			strings.write_string(str_b, "rax ; mov calculated value into variable: ")
			strings.write_string(str_b, statement_t.identifier)
			strings.write_string(str_b, "\n\n")
		case .AST_IF:
			generate_asm_for_if(str_b, statement_node, func_scope, file_info) or_return
		case .AST_WHILE:
			generate_asm_for_while(str_b, statement_node, func_scope, file_info) or_return
		case .AST_FOR:
			generate_asm_for_for(str_b, statement_node, func_scope, file_info) or_return
		case .AST_BREAK:
			strings.write_string(str_b, "\tjmp ")
			strings.write_string(str_b, func_scope.break_label)
			strings.write_string(str_b, "\t; Break\n\n")
	}

	return true
}

@(private="file")
clone_variables :: proc(p_scope: map[string]string) -> ^map[string]string {
	scope := new(map[string]string)

	for binding in p_scope {
		scope[binding] = p_scope[binding]
	}

	return scope
}

@(private="file")
clone_func_scope :: proc(func_scope: ^FunctionScope) -> (ret: ^FunctionScope) {
	ret = new(FunctionScope)
	ret.variables = clone_variables(func_scope.variables)^
	ret.label = func_scope.label

	ret.rbp_offset = new(int)
	ret.rbp_offset^ = func_scope.rbp_offset^

	ret.label_count = func_scope.label_count

	ret.break_label = strings.clone(func_scope.break_label)

	return ret
}

@(private="file")
generate_asm_for_scope :: proc(str_b: ^strings.Builder, scope_node: ^AstNode, 
							   func_scope: ^FunctionScope, file_info: ^FileInfo) -> bool {

	func_scope_clone := clone_func_scope(func_scope)

	for child in scope_node.childs {
		if child.type == .AST_SCOPE {
			generate_asm_for_scope(str_b, child, func_scope_clone, file_info) or_return
		} else {
			generate_asm_for_statement(str_b, child, func_scope_clone, file_info) or_return
		}
	}
	return true
}

@(private="file")
generate_asm_for_function :: proc(str_b: ^strings.Builder, function_node: ^AstNode, file_info: ^FileInfo) -> bool {

	function_t := function_node.value.(AstFunction)

	function_label := function_t.identifier
	if function_label == "main" do function_label = "start"

	strings.write_string(str_b, function_label)
	strings.write_string(str_b, ":\n")

	strings.write_string(str_b, "\tpush rbp\t; Save old base pointer\n")
	strings.write_string(str_b, "\tmov rbp, rsp\t; Set new Base pointer\n\n")

	func_scope := FunctionScope {
		variables = make(map[string]string),
		label_count = new(int),
		rbp_offset = new(int),
		label = strings.clone(function_label)
	}

	rbp_offset: int = 0

	parameter_rbp_offset: int = 8
	#reverse for child in function_node.childs {
		if child.type != .AST_VAR_DECLARE do continue

		statement_t := child.value.(AstStatement)
		parameter_rbp_offset += 8
		func_scope.variables[statement_t.identifier] = strings.clone(fmt.tprintf("[rbp+%d]", parameter_rbp_offset))
	}

	for child in function_node.childs {
		if child.type == .AST_SCOPE {
			if !generate_asm_for_scope(str_b, child, &func_scope, file_info) do return false
		}
	}

	return true
}

@(private="package")
collect_metadata_function :: proc(file_info: ^FileInfo, node: ^AstNode) -> bool {

	function_info: FunctionInfo
	function_t: AstFunction = node.value.(AstFunction)

	function_info.identifier = function_t.identifier
	function_info.return_type = function_t.ret_type
	for parameter in node.childs {
		if parameter.type != .AST_VAR_DECLARE do break

		statement_t := parameter.value.(AstStatement)
		append(&function_info.params, statement_t.type)
	}

	file_info.callables[function_t.identifier] = function_info
	return true
}

@(private="file")
generate_for_ast_node :: proc(str_b: ^strings.Builder, node: ^AstNode, file_info: ^FileInfo) -> bool {
	#partial switch node.type {
		case .AST_PROGRAM:
			for child in node.childs {
				if !generate_for_ast_node(str_b, child, file_info) do return false
			}
		case .AST_FUNCTION:
			if !collect_metadata_function(file_info, node) do return false
			if !generate_asm_for_function(str_b, node, file_info) do return false
	}
	return true
}

@(private="package")
generate_asm :: proc(ast: ^AstNode) -> string {
	str_b := strings.builder_make()
	defer strings.builder_destroy(&str_b)

	file_info: FileInfo

	append_fasm_header(&str_b)
	if !generate_for_ast_node(&str_b, ast, &file_info) do return ""

	return strings.clone(strings.to_string(str_b))
}

@(private="package")
compile_asm :: proc(src_name: string, bin_name: string) -> bool {
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
		stderr_fmt := fmt.tprintf("%s%s%s", RED, transmute(string)stderr, RESET)
		fmt.println(stderr_fmt)
		return false
	}

	if err != nil {
		log(.Error, "FASM could not be started!\nFasm is a dependency of this C compiler:\nhttps://flatassembler.net/")
		return false
	}

	log(.Proto, "Compiling of file ", bin_name, " was successful!")

	// r-xr-xr-x
	if os2.chmod(bin_name, 0o755) != nil {
		log(.Error, "File rights of ", bin_name, " could not be set properly!")
		return false
	}

	return true
}
