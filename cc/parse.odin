package cc

import "core:fmt"
import "core:strings"
import "core:strconv"

@(private="package")
NodeType :: enum {
	AST_PROGRAM = 0,
	AST_FUNCTION,
	AST_RETURN,
	AST_CONSTANT,
	AST_RETURN_STATEMENT,
	AST_VAR_DECLARE,
	AST_VAR_ASSIGNMENT,
	AST_EXPR_STATEMENT,
	AST_EXPR_CONSTANT,
	AST_EXPR_UNARY,
	AST_EXPR_BINARY,
	AST_EXPR_VARIABLE,
}

@(private="package")
AstNode :: struct {
	childs: [dynamic]^AstNode,
	parent: ^AstNode,
	type:   NodeType,
	value:  AstValue,
}

@(private="package")
AstValue :: union {
	AstFunction,
	AstStatement,
	AstExpression,
}

@(private="package")
AstFunction :: struct {
	ret_type:   AstDataType,
	identifier: string,
	params:     string,
}

@(private="package")
AstStatement :: struct {
	identifier:   string,
	value:        string
}

@(private="package")
AstExpression :: struct {
	value: string,
	operator: Operator,
}

@(private="package")
AstDataType :: enum {
	Void,
	Int,
}

@(private="file")
TokenIter :: struct {
	tokens: []Token,
	i:      int,
}

@(private="file")
Operator :: enum {
	OP,
	OP_UNARY_MINUS,
	OP_LOGICAL_NOT,
	OP_BIT_NEGATION,
	OP_BINARY_PLUS,
	OP_BINARY_MINUS,
	OP_BINARY_MULT,
	OP_BINARY_DIV,
	OP_BINARY_LESS,
	OP_BINARY_LESS_EQUAL,
	OP_BINARY_GREATER,
	OP_BINARY_GREATER_EQUAL,
	OP_BINARY_EQUAL,
	OP_BINARY_NOT_EQUAL,
}

@(private="package")
append_ast_node :: proc(parent: ^AstNode, child: ^AstNode) {
	append(&parent.childs, child)
	child.parent = parent
}

@(private="file")
next_token :: proc(iter: ^TokenIter) -> ^Token {
	iter.i += 1
	if iter.i >= len(iter.tokens) {
		return nil
	}
	return &iter.tokens[iter.i]
}

@(private="package")
current_token :: proc(iter: ^TokenIter) -> ^Token {
	return &iter.tokens[iter.i]
}

@(private="package")
prev_token :: proc(iter: ^TokenIter) -> ^Token {
	if iter.i - 1 < 0 do panic("prev_token called with first token!")
	return &iter.tokens[iter.i - 1]
}

@(private="package")
cleanup_ast_function :: proc(function_t: AstFunction) {
	delete(function_t.identifier)
	delete(function_t.params)
}

@(private="package")
cleanup_ast_statement :: proc(statement_t: AstStatement) {
	delete(statement_t.value)
	delete(statement_t.identifier)
}

@(private="package")
cleanup_ast_expression :: proc(expression_t: AstExpression) {
	delete(expression_t.value)
}

@(private="package")
cleanup_ast_node :: proc(root: ^AstNode) {
	for &child in root.childs {
		cleanup_ast_node(child)
	}
	switch v in root.value {
		case AstFunction:
			cleanup_ast_function(v)
		case AstStatement:
			cleanup_ast_statement(v)
		case AstExpression:
			cleanup_ast_expression(v)
	}
	if (root.childs != nil) do delete(root.childs)
	free(root)
}

@(private="file")
get_operator_for_token :: proc(token: ^Token, unary: bool) -> (Operator, bool) {
	#partial switch token.type {
		case .T_MINUS:			return unary ? .OP_UNARY_MINUS : .OP_BINARY_MINUS, true
		case .T_PLUS:			return .OP_BINARY_PLUS, true
		case .T_STAR:			return .OP_BINARY_MULT, true
		case .T_FSLASH:			return .OP_BINARY_DIV, true
		case .T_TILDE:			return .OP_BIT_NEGATION, true
		case .T_EXCLAMATION:	return .OP_LOGICAL_NOT, true
		case .T_GREATER:		return .OP_BINARY_GREATER, true
		case .T_GREATER_EQUAL:	return .OP_BINARY_GREATER_EQUAL, true
		case .T_LESS:			return .OP_BINARY_LESS, true
		case .T_LESS_EQUAL:		return .OP_BINARY_LESS_EQUAL, true
		case .T_EQUAL_EQUAL:	return .OP_BINARY_EQUAL, true
		case .T_NOT_EQUAL:		return .OP_BINARY_NOT_EQUAL, true
		case: return nil, false
	}
}

@(private="file")
is_token_unary_operator :: proc(token: ^Token) -> bool {
	#partial switch token.type {
		case .T_EXCLAMATION: fallthrough
		case .T_TILDE	  : fallthrough
		case .T_MINUS	  :
			return true
	}
	return false
}

@(private="file")
resolve_expr_primary :: proc(iter: ^TokenIter) -> (node: ^AstNode, ok: bool) {
	
	if current_token(iter).type == .T_OPEN_PARANTHESIS {
		next_token(iter)
		node, ok = resolve_expr(nil, iter)
		if !ok do return nil, false
	   
		if current_token(iter).type != .T_CLOSE_PARANTHESIS {
			log(.Error, "Expected ')' but found:", fmt.tprintf("%s", current_token(iter).type))
			return nil, false
		}
		next_token(iter)
		return node, true
	}

	if current_token(iter).type == .T_INT_LITERAL {
		ret := new(AstNode)
		expression_t: AstExpression
		expression_t.value = strings.clone(current_token(iter).value)
		ret.value = expression_t
		ret.type = .AST_EXPR_CONSTANT
		
		next_token(iter)
		return ret, true
	}

	if current_token(iter).type == .T_IDENTIFIER {
		ret := new(AstNode)
		expression_t: AstExpression
		expression_t.value = strings.clone(current_token(iter).value)
		ret.value = expression_t
		ret.type = .AST_EXPR_VARIABLE

		next_token(iter)
		return ret, true
	}
	
	if is_token_unary_operator(current_token(iter)) {
		ret := new(AstNode)
		expression_t: AstExpression
		ok: bool
		expression_t.operator = get_operator_for_token(current_token(iter), true) or_return

		ret.value = expression_t
		ret.type = .AST_EXPR_UNARY

		next_token(iter)
		node: ^AstNode
		node = resolve_expr_primary(iter) or_return
		append_ast_node(ret, node)
		return ret, true
	}

	log_error_with_token(current_token(iter)^, "Could not resolve primary expression")
	
	return nil, false
}

@(private="file")
resolve_expr_dot :: proc(parent: ^AstNode, iter: ^TokenIter) -> (left_child: ^AstNode, ok: bool) {
	
	left_child = resolve_expr_primary(iter) or_return
	
	for current_token(iter).type == .T_STAR || current_token(iter).type == .T_FSLASH {
		op_token := current_token(iter)
		next_token(iter)
		
		expression_t: AstExpression
		expression_t.operator = get_operator_for_token(op_token, false) or_return
		
		right_child := resolve_expr_primary(iter) or_return
		
		binary_node := new(AstNode)
		binary_node.type = .AST_EXPR_BINARY
		binary_node.value = expression_t
		
		append_ast_node(binary_node, left_child)
		append_ast_node(binary_node, right_child)
		
		left_child = binary_node
	}
	
	return left_child, true
}

@(private="file")
resolve_expr_additive :: proc(parent: ^AstNode, iter: ^TokenIter) -> (left_child: ^AstNode, ok: bool) {
	
	left_child = resolve_expr_dot(parent, iter) or_return
	
	for current_token(iter).type == .T_PLUS || current_token(iter).type == .T_MINUS {
		op_token := current_token(iter)
		next_token(iter)
		
		expression_t: AstExpression
		expression_t.operator = get_operator_for_token(op_token, false) or_return
		
		right_child := resolve_expr_dot(nil, iter) or_return
		
		binary_node := new(AstNode)
		binary_node.type = .AST_EXPR_BINARY
		binary_node.value = expression_t
		
		append_ast_node(binary_node, left_child)
		append_ast_node(binary_node, right_child)
		
		left_child = binary_node
	}
	
	return left_child, true
}

@(private="file")
resolve_expr_comparing :: proc(parent: ^AstNode, iter: ^TokenIter) -> (left_child: ^AstNode, ok: bool) {
	
	left_child = resolve_expr_additive(parent, iter) or_return
	
	for current_token(iter).type == .T_GREATER || 
		current_token(iter).type == .T_LESS || 
		current_token(iter).type == .T_LESS_EQUAL || 
		current_token(iter).type == .T_GREATER_EQUAL {

		op_token := current_token(iter)
		next_token(iter)
		
		expression_t: AstExpression
		expression_t.operator = get_operator_for_token(op_token, false) or_return
		
		right_child := resolve_expr_additive(nil, iter) or_return
		
		binary_node := new(AstNode)
		binary_node.type = .AST_EXPR_BINARY
		binary_node.value = expression_t
		
		append_ast_node(binary_node, left_child)
		append_ast_node(binary_node, right_child)
		
		left_child = binary_node
	}
	
	return left_child, true
}

@(private="file")
resolve_expr_equal :: proc(parent: ^AstNode, iter: ^TokenIter) -> (left_child: ^AstNode, ok: bool) {
	
	left_child = resolve_expr_comparing(parent, iter) or_return
	
	for current_token(iter).type == .T_EQUAL_EQUAL || 
		current_token(iter).type == .T_NOT_EQUAL {

		op_token := current_token(iter)
		next_token(iter)
		
		expression_t: AstExpression
		expression_t.operator = get_operator_for_token(op_token, false) or_return
		
		right_child := resolve_expr_comparing(nil, iter) or_return
		
		binary_node := new(AstNode)
		binary_node.type = .AST_EXPR_BINARY
		binary_node.value = expression_t
		
		append_ast_node(binary_node, left_child)
		append_ast_node(binary_node, right_child)
		
		left_child = binary_node
	}
	
	return left_child, true
}

@(private="package")
resolve_expr :: proc(root: ^AstNode, iter: ^TokenIter) -> (^AstNode, bool) {
	return resolve_expr_equal(root, iter)
}

@(private="package")
resolving_assignment :: proc(root: ^AstNode, iter: ^TokenIter, identifier: string) -> bool {
	statement_t: AstStatement
	statement_t.identifier = strings.clone(prev_token(iter).value)
	root.value = statement_t
	root.type = .AST_VAR_ASSIGNMENT
	next_token(iter)

	node, ok := resolve_expr(root, iter)
	if !ok {
		log(.Error, "Could not resolve expression")
		return false
	}
	append_ast_node(root, node)

	return true
}

@(private="package")
resolve_statement :: proc(root: ^AstNode, iter: ^TokenIter) -> (^AstNode, bool) {
	statement_t: AstStatement
	node := new(AstNode)
	node.value = statement_t
	parsed_stmt := false

	if current_token(iter) == nil {
		log(.Error, "Ran out of tokens whilst resolving statement: Aborting")
		return node, false
	}

	if !parsed_stmt && current_token(iter).type == .T_RETURN_KEYWORD {
		next_token(iter)
		expr_node, ok := resolve_expr(node, iter)
		if !ok {
			log_error_with_token(current_token(iter)^, "Return statement does not end with valid expression")
			if node != nil do cleanup_ast_node(node)
			return node, false
		}
		node.type = .AST_RETURN_STATEMENT
		append_ast_node(node, expr_node)
		parsed_stmt = true
	}

	if !parsed_stmt && current_token(iter).type == .T_INT_KEYWORD {
		next_token(iter)
		parsed_stmt = true
		if current_token(iter).type != .T_IDENTIFIER {
			log_error_with_token(current_token(iter)^, "Expected identifier after int keyword")
			if node != nil do cleanup_ast_node(node)
			return node, false
		}

		identifier := current_token(iter).value
		node.type = .AST_VAR_DECLARE
		statement_t.identifier = strings.clone(identifier)
		node.value = statement_t

		next_token(iter)
		if current_token(iter).type == .T_ASSIGNMENT {
			assignment_node := new(AstNode)
			if !resolving_assignment(assignment_node, iter, identifier) {
				if node != nil do cleanup_ast_node(node)
				return node, false
			}
			append_ast_node(node, assignment_node)
		}
	}

	if !parsed_stmt && current_token(iter).type == .T_IDENTIFIER {
		parsed_stmt = true
		node.type = .AST_VAR_ASSIGNMENT
		next_token(iter)
		if current_token(iter).type != .T_ASSIGNMENT {
			log_error_with_token(current_token(iter)^, "Expected = Token while got ")
			if node != nil do cleanup_ast_node(node)
			return node, false
		}

		identifier := current_token(iter).value
		if !resolving_assignment(node, iter, identifier) {
			if node != nil do cleanup_ast_node(node)
			return node, false
		}
	}

	if !parsed_stmt {
		expr_node, ok := resolve_expr(node, iter)
		if ok {
			append_ast_node(node, expr_node)
			parsed_stmt = true
			node.type = .AST_EXPR_STATEMENT
		}
	}


	if current_token(iter).type != .T_SEMICOLON {
		log_error_with_token(current_token(iter)^, "Statement has to end with ;")
		if node != nil do cleanup_ast_node(node)
		return node, false
	}

	return node, parsed_stmt
}

@(private="package")
resolve_function :: proc(root: ^AstNode, iter: ^TokenIter) -> (node: ^AstNode, ok: bool) {
	node = new(AstNode)
	node.type = .AST_FUNCTION

	function_t: AstFunction

	// int
	if current_token(iter).type != .T_INT_KEYWORD {
		log_error_with_token(current_token(iter)^, "Token is not a valid return type")
		return node, false
	}
	function_t.ret_type = .Int

	next_token(iter)
	
	// int foo
	if current_token(iter).type != .T_IDENTIFIER {
		log_error_with_token(current_token(iter)^, "Token is not a valid identifier")
		return node, false
	}
	function_t.identifier = strings.clone(current_token(iter).value)

	next_token(iter)

	// int foo(
	if current_token(iter).type != .T_OPEN_PARANTHESIS {
		log_error_with_token(current_token(iter)^, "Expected \'(\' after function identifier")
		return node, false
	}

	next_token(iter)

	// int foo(void
	if current_token(iter).type != .T_VOID_KEYWORD {
		log_error_with_token(current_token(iter)^, "Function parameters have to be void for now" )
		return node, false
	}
	function_t.params = strings.clone(current_token(iter).value)

	next_token(iter)

	// int foo(void)
	if current_token(iter).type != .T_CLOSE_PARANTHESIS {
		log_error_with_token(current_token(iter)^, "Expected \')\' after function parameter")
		return node, false
	}

	next_token(iter)

	// int foo(void) {
	if current_token(iter).type != .T_OPEN_BRACE {
		log_error_with_token(current_token(iter)^, "Expected \'{\' after function parameters")
		return node, false
	}

	next_token(iter)

	// int foo(void) {...}
	for current_token(iter).type != .T_CLOSE_BRACE {
		statement_node := resolve_statement(node, iter) or_return

		append_ast_node(node, statement_node)
		next_token(iter)
	} 

	node.value = function_t
	return node, true
}

@(private="package")
build_ast :: proc(tokens: []Token) -> (bool, ^AstNode) {
	root := new(AstNode)

	iter: TokenIter = {
		tokens = tokens
	}

	for iter.i < len(iter.tokens) {
		node, ok := resolve_function(root, &iter)
		append_ast_node(root, node)
		if !ok {
			log(.Error, "Could not resolve function: Aborting")
			return false, root
		}
		next_token(&iter)
	}

	return true, root
}
