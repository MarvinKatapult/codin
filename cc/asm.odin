package cc

import "core:strings"
import "core:fmt"
import "core:os/os2"
import "core:sys/linux/"

@(private="file")
PTR_SIZE :: 4
ENTRY_LABEL :: "main"

@(private="file")
FunctionInfo :: struct {
	params:      [dynamic]DataType,
	identifier:  string,
	return_type: DataType,
	implemented: bool,
}

@(private="file")
FileInfo :: struct {
	callables: map[string]FunctionInfo
}

@(private="file")
FunctionScope :: struct {
	variables:   map[string]Variable,
	label:       string,
	label_count: ^int,
	ebp_offset:  ^int,
	break_label: string,
}

Variable :: struct {
	type:       DataType,
	ebp_offset: string,
}

@(private="file")
append_nasm_header :: proc(str_b: ^strings.Builder) {
	strings.write_string(str_b, "section .text\n")
	strings.write_string(str_b, fmt.tprint("\tglobal ", ENTRY_LABEL, "\n\n"))
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
	strings.write_string(str_b, "\tcmp eax, 0\t; IF Check if condition is false\n")
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
			strings.write_string(str_b, "\tcmp eax, 0\t; ELSE-IF Check if condition is false\n")
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

	strings.write_string(str_b, "\tcmp eax, 0\t; Check if condition is false\n")
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

		strings.write_string(str_b, "\tcmp eax, 0\t; Check if condition is false\n")
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
generate_asm_for_operator :: proc(str_b: ^strings.Builder, expression_node: ^AstNode, 
								  func_scope: ^FunctionScope) -> bool {

	expression_t := expression_node.value.(AstExpression)

	switch expression_t.operator {
		case .OP_BIT_NEGATION:
			strings.write_string(str_b, "\tnot eax\t\t; negating Value in EAX (~)\n")
		case .OP_LOGICAL_NOT:
			strings.write_string(str_b, "\txor eax, 1\t; Flip LSB true <-> false\n")
		case .OP_UNARY_MINUS:
			strings.write_string(str_b, "\tneg eax\t\t; negating Value in EAX (~)\n")
		case .OP_BINARY_PLUS:
			strings.write_string(str_b, "\tadd eax, edi\t; Adding eax and edi\n")
		case .OP_BINARY_MINUS:
			strings.write_string(str_b, "\tsub edi, eax\t; Subtracting edi from eax\n")
			strings.write_string(str_b, "\tmov eax, edi\t; Moving result in eax\n")
		case .OP_BINARY_MULT:
			strings.write_string(str_b, "\tmul edi\t\t; Multiplying edi with eax\n")
		case .OP_BINARY_DIV:
			strings.write_string(str_b, "\tmov edx, edi	; Moving edi to edx\n")
			strings.write_string(str_b, "\tmov ecx, eax	; Moving eax to ecx\n")
			strings.write_string(str_b, "\tmov eax, edx	; moving edx in eax\n")
			strings.write_string(str_b, "\tcdq\n")
			strings.write_string(str_b, "\tidiv ecx\t; EAX = EAX / ECX\n")
		case .OP_BINARY_MOD:
			strings.write_string(str_b, "\tmov edx, edi	; Moving edi to edx\n")
			strings.write_string(str_b, "\tmov ecx, eax	; Moving eax to ecx\n")
			strings.write_string(str_b, "\tmov eax, edx	; moving edx in eax\n")
			strings.write_string(str_b, "\tcdq\n")
			strings.write_string(str_b, "\tidiv ecx\t; EAX = EAX / ECX\n")
			strings.write_string(str_b, "\tmov eax, edx\t; Move remainder into eax\n")
		case .OP_BINARY_LESS:
			strings.write_string(str_b, "\tcmp edi, eax\t; Compare eax < edi\n")
			strings.write_string(str_b, "\tsetl al\t\t; Set al to 1 if true\n")
			strings.write_string(str_b, "\tmovsx eax, al\t; Extend al to eax\n")
		case .OP_BINARY_LESS_EQUAL:
			strings.write_string(str_b, "\tcmp edi, eax\t; Compare eax <= edi\n")
			strings.write_string(str_b, "\tsetle al\t\t; Set al to 1 if true\n")
			strings.write_string(str_b, "\tmovsx eax, al\t; Extend al to eax\n")
		case .OP_BINARY_GREATER:
			strings.write_string(str_b, "\tcmp edi, eax\t; Compare eax > edi\n")
			strings.write_string(str_b, "\tsetg al\t\t; Set al to 1 if true\n")
			strings.write_string(str_b, "\tmovsx eax, al\t; Extend al to eax\n")
		case .OP_BINARY_GREATER_EQUAL:
			strings.write_string(str_b, "\tcmp edi, eax\t; Compare eax >= edi\n")
			strings.write_string(str_b, "\tsetge al\t\t; Set al to 1 if true\n")
			strings.write_string(str_b, "\tmovsx eax, al\t; Extend al to eax\n")
		case .OP_BINARY_EQUAL:
			strings.write_string(str_b, "\tcmp edi, eax\t; Compare eax == edi\n")
			strings.write_string(str_b, "\tsete al\t\t; Set al to 1 if true\n")
			strings.write_string(str_b, "\tmovsx eax, al\t; Extend al to eax\n")
		case .OP_BINARY_NOT_EQUAL:
			strings.write_string(str_b, "\tcmp edi, eax\t; Compare eax != edi\n")
			strings.write_string(str_b, "\tsetne al\t; Set al to 1 if true\n")
			strings.write_string(str_b, "\tmovsx eax, al\t; Extend al to eax\n")
		case .OP_LOGICAL_OR:
			strings.write_string(str_b, "\ttest edi, edi\t; Check if edi != 0\n")
			strings.write_string(str_b, "\tsetnz dl\t; dl = (edi != 0) ? 1 : 0\n")
			strings.write_string(str_b, "\tjnz ")
			write_label(str_b, func_scope)
			strings.write_string(str_b, "\t\t; Short Circuit Evaluation\n")
			strings.write_string(str_b, "\ttest eax, eax\t; Check if eax != 0\n")
			strings.write_string(str_b, "\tsetnz al \t; al = 1 if 0F == 1 else = 0\n")
			strings.write_string(str_b, "\tor dl, al\t; Logical OR\n\t")
			write_label(str_b, func_scope, advance = true)
			strings.write_string(str_b, ":\n")
			strings.write_string(str_b, "\tmovzx eax, dl\t; -> 64-Bit\n")
		case .OP_LOGICAL_AND:
			strings.write_string(str_b, "\ttest edi, edi\t; Check if edi != 0\n")
			strings.write_string(str_b, "\tsetnz dl\t; dl = (edi != 0) ? 1 : 0\n")
			strings.write_string(str_b, "\tjz ")
			write_label(str_b, func_scope)
			strings.write_string(str_b, "\t\t; Short Circuit Evaluation\n")
			strings.write_string(str_b, "\ttest eax, eax\t; Check if eax != 0\n")
			strings.write_string(str_b, "\tsetnz al \t; al = 1 if 0F == 1 else = 0\n")
			strings.write_string(str_b, "\tand dl, al\t; Logical AND\n\t")
			write_label(str_b, func_scope, advance = true)
			strings.write_string(str_b, ":\n")
			strings.write_string(str_b, "\tmovzx eax, dl\t; -> 64-Bit\n")
		case .OP_BIT_OR:
			strings.write_string(str_b, "\tor eax, edi\t; Bit OR\n")
		case .OP_BIT_AND:
			strings.write_string(str_b, "\tand eax, edi\t; Bit AND\n")
		case .OP_BIT_XOR:
			strings.write_string(str_b, "\txor eax, edi\t; Bit XOR\n")
		case .OP_BIT_SHL:
			strings.write_string(str_b, "\tmov ecx, eax\t; Only cl can be used for shift operations\n")
			strings.write_string(str_b, "\tsal edi, cl\t; eax << edi\n")
			strings.write_string(str_b, "\tmov eax, edi\t\n")
		case .OP_BIT_SHR:
			strings.write_string(str_b, "\tmov ecx, eax\t; Only cl can be used for shift operations\n")
			strings.write_string(str_b, "\tsar edi, cl\t; eax >> edi\n")
			strings.write_string(str_b, "\tmov eax, edi\t\n")
		case .OP_ADRESS:
			if expression_node.childs[0].type != .AST_EXPR_VARIABLE {
				log(.Error, "Adress Operator can only be used with a variable")
				return false
			}
			var_expr_t := expression_node.childs[0].value.(AstExpression)
			var := func_scope.variables[var_expr_t.value]

			strings.write_string(str_b, fmt.tprintf("\tlea eax, %s\t; Load adress of variable\n", var.ebp_offset))
		case .OP_DEREFERENCE:
			if expression_node.childs[0].type != .AST_EXPR_VARIABLE {
				log(.Error, "Adress Operator can only be used with a variable")
				return false
			}

			strings.write_string(str_b, "\tmov eax, [eax]\t; Dereference variable\n")
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

			if expression_t.value not_in func_scope.variables {
				log(.Error, "Variable ", expression_t.value, " not declared!")
				return false
			}

			var := func_scope.variables[expression_t.value]
			write_str := fmt.tprintf(
				"\tmov %s, %s %s ; moving value of variable %s directly into eax\n\n",
				register_for_type(var.type), size_keyword_for_type(var.type), var.ebp_offset, expression_t.value
			)

			strings.write_string(str_b, write_str)
			return true
		case .AST_EXPR_CONSTANT:
			if len(expression_node.childs) != 0 {
				log(.Error, "Constant Expression Type cannot have childs")
				return false
			}
			strings.write_string(str_b, "\tmov eax, ")
			strings.write_string(str_b, expression_t.value)
			strings.write_string(str_b, "\t; moving value of expression directly into eax\n\n")
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
		
			strings.write_string(str_b, "\tpush eax\t; Push to stack\n\n")

			valuer := expression_node.childs[1]
			if !generate_asm_for_expr(str_b, valuer, func_scope, file_info) do return false

			strings.write_string(str_b, "\tpop edi\t\t; Popping Value to edi off stack\n\n")

			return generate_asm_for_operator(str_b, expression_node, func_scope)
		case .AST_FUNC_CALL:

			expression_t := expression_node.value.(AstExpression)
			func_identifier := expression_t.value
			
			if func_identifier not_in file_info.callables {
				log(.Error, fmt.tprintf("Function %s not found", func_identifier))
				return false
			}

			func_data := file_info.callables[func_identifier]

			if !func_data.implemented && false {
				log(.Error, fmt.tprintf("Function %s is not implemented", func_identifier))
				return false
			}

			if len(func_data.params) != len(expression_node.childs) {
				log(.Error, fmt.tprintf("Call to function %s mismatching parameter count", func_identifier))
				return false
			}

			// Put parameters for call on stack
			original_ebp_offset := func_scope.ebp_offset^
			#reverse for child, i in expression_node.childs {
				generate_asm_for_expr(str_b, child, func_scope, file_info) or_return

				strings.write_string(str_b, 
					fmt.tprintf("\tpush eax\t; Allocate memory on the stack for parameter\n\n")
				)

				ebp_offset := func_scope.ebp_offset
				ebp_offset^ = ebp_offset^ - PTR_SIZE
			}
			
			strings.write_string(str_b, "\tcall ")
			strings.write_string(str_b, expression_t.value)
			strings.write_string(str_b, "\t; Call Function\n\n")

			// Reset Memory from parameters on stack
			mem_to_free_from_stack := abs(func_scope.ebp_offset^ - original_ebp_offset)
			if mem_to_free_from_stack > 0 {
				strings.write_string(str_b, "\tadd esp, ")
				strings.write_int(str_b, mem_to_free_from_stack)
				strings.write_string(str_b, "\t; Resetting Stack for func call\n\n")

				func_scope.ebp_offset^ += mem_to_free_from_stack
			}

			return true
	}

	log(.Error, "Expression must be constant or unary expression!")

	return false
}

@(private="package")
generate_asm_for_var_declare :: proc(str_b: ^strings.Builder, statement_node: ^AstNode, 
									 func_scope: ^FunctionScope, file_info: ^FileInfo) -> bool {

	statement_t := statement_node.value.(AstStatement)
	if statement_t.identifier in func_scope.variables {
		log(.Error, "Variale redefinition!")
		return false
	}

	strings.write_string(str_b, "\tsub esp, ")
	strings.write_int(str_b,	statement_t.type.size)
	strings.write_string(str_b, "\t; Allocate memory on the stack\n\n")

	ebp_offset := func_scope.ebp_offset
	ebp_offset^ = ebp_offset^ - statement_t.type.size

	variable := Variable{type = statement_t.type, ebp_offset = strings.clone(fmt.tprintf("[ebp%d]", ebp_offset^))}
	func_scope.variables[statement_t.identifier] = variable

	if len(statement_node.childs) > 0 {
		if !generate_asm_for_statement(str_b, statement_node.childs[0], func_scope, file_info) {
			return false
		}
	}
	return true
}

@(private="file")
register_for_type :: proc(type: DataType) -> string {
	switch type.size {
		case 1: return "al"
		case 2: return "ax"
		case 4: return "eax"
	}

	assert(false)
	return "";
}

@(private="file")
size_keyword_for_type :: proc(type: DataType) -> string {
	switch type.size {
		case 1: return "byte"
		case 2: return "word"
		case 4: return "dword"
		case 8: return "qword"
	}

	assert(false)
	return "";
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
				strings.write_string(str_b, "\tret \t\t; Returning\n\n")
			}
		case .AST_VAR_DECLARE:
			generate_asm_for_var_declare(str_b, statement_node, func_scope, file_info) or_return
		case .AST_VAR_DEREF_ASSIGNMENT: fallthrough
		case .AST_VAR_ASSIGNMENT:
			if len(statement_node.childs) <= 0 do return false
			ok := generate_asm_for_expr(str_b, statement_node.childs[0], func_scope, file_info)
			if !ok do return false

			statement_t := statement_node.value.(AstStatement)
			
			if statement_t.identifier not_in func_scope.variables {
				log(.Error, "Variable ", statement_t.identifier, " not declared!")
				return false
			}
			var: Variable = func_scope.variables[statement_t.identifier]

			mov_string: string
			if statement_node.type == .AST_VAR_DEREF_ASSIGNMENT {
				strings.write_string(str_b, fmt.tprintf(
						"\tmov ebx, %s %s\t; Mov ptr value of %s into edi\n",
						size_keyword_for_type(var.type), var.ebp_offset, statement_t.identifier
					)
				)
				mov_string = fmt.tprintf(
					"\tmov %s [ebx], %s \t; mov calculated value into variable: %s\n\n",
					size_keyword_for_type(var.type), register_for_type(var.type), statement_t.identifier
				)
			} else {
				mov_string = fmt.tprintf(
					"\tmov %s %s, %s \t; mov calculated value into variable: %s\n\n",
					size_keyword_for_type(var.type), var.ebp_offset, register_for_type(var.type), statement_t.identifier
				)
			}
			strings.write_string(str_b, mov_string)
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
clone_variables :: proc(p_scope: map[string]Variable) -> ^map[string]Variable {
	scope := new(map[string]Variable)

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

	ret.ebp_offset = new(int)
	ret.ebp_offset^ = func_scope.ebp_offset^

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
	// This was actually relevant before, because we used the _start entry point
	if function_label == "main" do function_label = ENTRY_LABEL 
	// Now this is not really relevant but I'm still going to leave this here, 
	// because I might want to change the entry point later

	strings.write_string(str_b, function_label)
	strings.write_string(str_b, ":\n")

	strings.write_string(str_b, "\tpush ebp\t; Save old base pointer\n")
	strings.write_string(str_b, "\tmov ebp, esp\t; Set new Base pointer\n\n")

	func_scope := FunctionScope {
		variables = make(map[string]Variable),
		label_count = new(int),
		ebp_offset = new(int),
		label = function_label
	}

	ebp_offset: int = 0

	parameter_ebp_offset: int = PTR_SIZE
	for child in function_node.childs {
		if child.type != .AST_VAR_DECLARE do continue

		statement_t := child.value.(AstStatement)
		parameter_ebp_offset += PTR_SIZE
		variable := Variable{type = statement_t.type, ebp_offset = strings.clone(fmt.tprintf("[ebp+%d]", parameter_ebp_offset))}
		func_scope.variables[statement_t.identifier] = variable
	}

	for child in function_node.childs {
		if child.type == .AST_SCOPE {
			if !generate_asm_for_scope(str_b, child, &func_scope, file_info) do return false
		}
	}

	strings.write_string(str_b, "\tleave; Always leave and ret at end of function\n")
	strings.write_string(str_b, "\tret\n")

	return true
}

@(private="package")
collect_metadata_function :: proc(file_info: ^FileInfo, node: ^AstNode, implementation := false) -> bool {

	function_info: FunctionInfo
	function_info.implemented = implementation
	function_t: AstFunction = node.value.(AstFunction)

	function_info.identifier = function_t.identifier
	function_info.return_type = function_t.ret_type
	for parameter in node.childs {
		if parameter.type != .AST_VAR_DECLARE do break

		statement_t := parameter.value.(AstStatement)
		append(&function_info.params, statement_t.type)
	}

	if function_t.identifier in file_info.callables {
		log(.Error, fmt.tprintf("Function %s is duplicate", function_t.identifier))
		return false
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
			if !collect_metadata_function(file_info, node, implementation = true) do return false
			if !generate_asm_for_function(str_b, node, file_info) do return false
		case .AST_FUNCTION_DECLARE: 
			if !collect_metadata_function(file_info, node) do return false
			strings.write_string(str_b, fmt.tprintf("extern %s; Extern Reference\n\n", node.value.(AstFunction).identifier))
	}
	return true
}

@(private="package")
generate_asm :: proc(ast: ^AstNode) -> string {
	str_b := strings.builder_make()
	defer strings.builder_destroy(&str_b)

	file_info: FileInfo

	append_nasm_header(&str_b)
	if !generate_for_ast_node(&str_b, ast, &file_info) do return ""

	return strings.clone(strings.to_string(str_b))
}

@(private="package")
compile_asm :: proc(src_name: string) -> bool {
	obj_file_name := cc_flags.is_object ? cc_flags.output_file : fmt.tprintf("output/%s.o", cc_flags.output_file)
	p_desc: os2.Process_Desc = {
		command = {"nasm", src_name, "-f", "elf", "-o", obj_file_name},
	} 
	log(.Proto, fmt.tprintf("Command for Nasm:%s", fmt.tprint(p_desc.command)))
	process_state, stdout, stderr, err := os2.process_exec(
		p_desc,
		context.temp_allocator
	)

	log(.Proto, "Nasm Output:")
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
		log(.Error, "NASM could not be started!\nNasm is a dependency of this C compiler:\nhttps://www.nasm.us")
		return false
	}

	if !cc_flags.is_object {
		// Linking
		log(.Proto, "Linking of executeable")
		p_desc = {
			command = { 
				"ld", 
				"-m", "elf_i386", 
				"-o", cc_flags.output_file, 
				obj_file_name,
				"-lc", 
				"/usr/lib32/crt1.o", 
				"/usr/lib32/crti.o", 
				"/usr/lib32/crtn.o", 
				"-dynamic-linker", "/lib/ld-linux.so.2"
			}
		}
		log(.Proto, fmt.tprint(p_desc.command))
		process_state, stdout, stderr, err = os2.process_exec(
			p_desc,
			context.temp_allocator
		)

		if len(stderr) > 0 {
			stderr_fmt := fmt.tprintf("%s%s%s", RED, transmute(string)stderr, RESET)
			fmt.println(stderr_fmt)
			return false
		}

		if err != nil {
			log(.Error, "ld could not be started!\n")
			log(.Error, fmt.tprint(err))
			return false
		}
	}

	log(.Proto, "Compiling of file ", cc_flags.output_file, " was successful!")

	// r-xr-xr-x
	if !cc_flags.is_object && os2.chmod(cc_flags.output_file, 0o755) != nil {
		log(.Error, "File rights of ", cc_flags.output_file, " could not be set properly!")
		return false
	}

	return true
}
