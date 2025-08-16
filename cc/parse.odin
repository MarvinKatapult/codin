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
    AST_EXPRESSION_CONSTANT,
    AST_EXPRESSION_UNARY,
    AST_EXPRESSION_BINARY,
}

@(private="package")
AstValue :: union {
    AstFunction,
    AstStatement,
    AstExpression,
}

@(private="package")
AstNode :: struct {
    childs: [dynamic]^AstNode,
	parent: ^AstNode,
    type: NodeType,
    value: AstValue,
}

@(private="package")
AstFunction :: struct {
    ret_type: AstDataType,
    identifier: string,
    params: string,
}

@(private="package")
AstDataType :: enum {
    Int,
    Void,
}

@(private="package")
AstStatement :: struct {
	return_value: string,
}

@(private="package")
AstExpression :: struct {
    value: string,
    operator: Operator,
}

@(private="file")
TokenIter :: struct {
    tokens: []Token,
    i: int,
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
cleanup_ast_function :: proc(function_t: AstFunction) {
    log(.Debug, "Cleaning AstFunction")
    delete(function_t.identifier)
    delete(function_t.params)
}

@(private="package")
cleanup_ast_statement :: proc(statement_t: AstStatement) {
    log(.Debug, "Cleaning AstStatement")
	delete(statement_t.return_value)
}

@(private="package")
cleanup_ast_expression :: proc(expression_t: AstExpression) {
    log(.Debug, "Cleaning AstExpression")
    delete(expression_t.value)
}

@(private="package")
cleanup_ast_node :: proc(root: ^AstNode) {
    for &child in root.childs {
        log(.Debug, "Deleting Child of type: ", fmt.tprintf("%s", child.type))
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
get_operator_for_token :: proc(token: ^Token, unary: bool) -> (bool, Operator) {
	#partial switch token.type {
		case .T_MINUS:			return true, unary ? .OP_UNARY_MINUS : .OP_BINARY_MINUS
		case .T_PLUS:			return true, .OP_BINARY_PLUS
		case .T_STAR:			return true, .OP_BINARY_MULT
		case .T_FSLASH:			return true, .OP_BINARY_DIV
		case .T_TILDE:			return true, .OP_BIT_NEGATION
		case .T_EXCLAMATION:	return true, .OP_LOGICAL_NOT
		case .T_GREATER:		return true, .OP_BINARY_GREATER
		case .T_GREATER_EQUAL:	return true, .OP_BINARY_GREATER_EQUAL
		case .T_LESS:			return true, .OP_BINARY_LESS
		case .T_LESS_EQUAL:		return true, .OP_BINARY_LESS_EQUAL
		case .T_EQUAL_EQUAL:	return true, .OP_BINARY_EQUAL
		case .T_NOT_EQUAL:		return true, .OP_BINARY_NOT_EQUAL
		case: return false, nil
	}
}

@(private="file")
is_token_unary_operator :: proc(token: ^Token) -> bool {
	#partial switch token.type {
		case .T_EXCLAMATION: fallthrough
		case .T_TILDE      : fallthrough
		case .T_MINUS      :
			return true
	}
	return false
}

@(private="file")
resolve_expr_primary :: proc(iter: ^TokenIter) -> (bool, ^AstNode) {
    log(.Debug, "Call to resolve_expr_primary")
    
    if current_token(iter).type == .T_OPEN_PARANTHESIS {
        next_token(iter)
        ok, node := resolve_expr(nil, iter)
        if !ok do return false, nil
       
        if current_token(iter).type != .T_CLOSE_PARANTHESIS {
            log(.Error, "Expected ')' but found:", fmt.tprintf("%s", current_token(iter).type))
            return false, nil
        }
        next_token(iter)
        return true, node
    }

    if current_token(iter).type == .T_INT_LITERAL {
        ret := new(AstNode)
        expression_t: AstExpression
        expression_t.value = strings.clone(current_token(iter).value)
        ret.value = expression_t
        ret.type = .AST_EXPRESSION_CONSTANT
        
        next_token(iter)
        return true, ret
    }
    
	if is_token_unary_operator(current_token(iter)) {
		ret := new(AstNode)
		expression_t: AstExpression
		ok: bool
		ok, expression_t.operator = get_operator_for_token(current_token(iter), true)
		if !ok do return false, nil

		ret.value = expression_t
		ret.type = .AST_EXPRESSION_UNARY

		next_token(iter)
		node: ^AstNode
		ok, node = resolve_expr_primary(iter)
		append_ast_node(ret, node)
		return true, ret
	}
    
    return false, nil
}

@(private="file")
resolve_expr_dot :: proc(parent: ^AstNode, iter: ^TokenIter) -> (bool, ^AstNode) {
    log(.Debug, "Call to resolve_expr_dot")
    
    ok, left_child := resolve_expr_primary(iter)
    if !ok do return false, nil
    
    for current_token(iter).type == .T_STAR || current_token(iter).type == .T_FSLASH {
        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        ok, expression_t.operator = get_operator_for_token(op_token, false)
        if !ok do return false, nil
        
        ok, right_child := resolve_expr_primary(iter)
        if !ok do return false, nil
        
        binary_node := new(AstNode)
        binary_node.type = .AST_EXPRESSION_BINARY
        binary_node.value = expression_t
        
        append_ast_node(binary_node, left_child)
        append_ast_node(binary_node, right_child)
        
        left_child = binary_node
    }
    
    return true, left_child
}

@(private="file")
resolve_expr_additive :: proc(parent: ^AstNode, iter: ^TokenIter) -> (bool, ^AstNode) {
    log(.Debug, "Call to resolve_expr_additive")
    
    ok, left_child := resolve_expr_dot(parent, iter)
    if !ok do return false, nil
    
    for current_token(iter).type == .T_PLUS || current_token(iter).type == .T_MINUS {
        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        ok, expression_t.operator = get_operator_for_token(op_token, false)
        if !ok do return false, nil
        
        ok, right_child := resolve_expr_dot(nil, iter)
        if !ok do return false, nil
        
        binary_node := new(AstNode)
        binary_node.type = .AST_EXPRESSION_BINARY
        binary_node.value = expression_t
        
        append_ast_node(binary_node, left_child)
        append_ast_node(binary_node, right_child)
        
        left_child = binary_node
    }
    
    return true, left_child
}

@(private="file")
resolve_expr_comparing :: proc(parent: ^AstNode, iter: ^TokenIter) -> (bool, ^AstNode) {
    log(.Debug, "Call to resolve_expr_comparing")
    
    ok, left_child := resolve_expr_additive(parent, iter)
    if !ok do return false, nil
    
    for current_token(iter).type == .T_GREATER || 
	    current_token(iter).type == .T_LESS || 
		current_token(iter).type == .T_LESS_EQUAL || 
		current_token(iter).type == .T_GREATER_EQUAL {

        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        ok, expression_t.operator = get_operator_for_token(op_token, false)
        if !ok do return false, nil
        
        ok, right_child := resolve_expr_additive(nil, iter)
        if !ok do return false, nil
        
        binary_node := new(AstNode)
        binary_node.type = .AST_EXPRESSION_BINARY
        binary_node.value = expression_t
        
        append_ast_node(binary_node, left_child)
        append_ast_node(binary_node, right_child)
        
        left_child = binary_node
    }
    
    return true, left_child
}

@(private="file")
resolve_expr_equal :: proc(parent: ^AstNode, iter: ^TokenIter) -> (bool, ^AstNode) {
    log(.Debug, "Call to resolve_expr_equal")
    
    ok, left_child := resolve_expr_comparing(parent, iter)
    if !ok do return false, nil
    
    for current_token(iter).type == .T_EQUAL_EQUAL || 
	    current_token(iter).type == .T_NOT_EQUAL {

        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        ok, expression_t.operator = get_operator_for_token(op_token, false)
        if !ok do return false, nil
        
        ok, right_child := resolve_expr_comparing(nil, iter)
        if !ok do return false, nil
        
        binary_node := new(AstNode)
        binary_node.type = .AST_EXPRESSION_BINARY
        binary_node.value = expression_t
        
        append_ast_node(binary_node, left_child)
        append_ast_node(binary_node, right_child)
        
        left_child = binary_node
    }
    
    return true, left_child
}

@(private="package")
resolve_expr :: proc(root: ^AstNode, iter: ^TokenIter) -> (bool, ^AstNode) {
    log(.Debug, "Call to resolve_expr")
    return resolve_expr_equal(root, iter)
}

@(private="package")
resolve_statement :: proc(root: ^AstNode, iter: ^TokenIter) -> (bool, ^AstNode) {
	node := new(AstNode)
    statement_t: AstStatement

    token: ^Token
    for i := 0; ; i += 1 {
        node.value = statement_t

        if i != 0 do token = next_token(iter)
        else do token = &iter.tokens[iter.i]

        if token == nil {
            log(.Error, "Ran out of tokens whilst resolving statement: Aborting")
            return false, node
        }

        buf: [8]u8
        strconv.itoa(buf[:], i)
        switch i {
            case 0:
                if token.type != .T_RETURN_KEYWORD {
                    log_error_with_token(token^, "Statement has to begin with return keyword")
					cleanup_ast_node(node)
                    return false, node
                }
                node.type = .AST_RETURN_STATEMENT
            case 1:
                ok, node_expression := resolve_expr(node, iter)
                if !ok {
					cleanup_ast_node(node)
                    return false, node
                }
				append_ast_node(node, node_expression)
                if current_token(iter).type != .T_SEMICOLON {
                    log_error_with_token(token^, "Statement has to end with ;")
					cleanup_ast_node(node)
                    return false, node
                }
                
                return true, node
        }
    }

    return false, node
}

@(private="package")
resolve_function :: proc(root: ^AstNode, iter: ^TokenIter) -> (bool, ^AstNode) {

	node := new(AstNode)
    node.type = .AST_FUNCTION

    function_t: AstFunction
    
    for i := 0; ; i += 1 {
        token: ^Token
        node.value = function_t

        buf: [8]u8
        strconv.itoa(buf[:], i)

        if i != 0 do token = next_token(iter)
        else do token = &iter.tokens[iter.i]

        if token == nil {
            log(.Error, "Ran out of tokens whilst resolving function: Aborting!")
            return false, node
        }

        switch i {
            case 0:
                if token.type != .T_INT_KEYWORD {
                    log_error_with_token(token^, "Token is not a valid return type")
                    return false, node
                }
                function_t.ret_type = .Int

            case 1:
                if token.type != .T_IDENTIFIER {
                    log_error_with_token(token^, "Token is not a valid identifier")
                    return false, node
                }
                function_t.identifier = strings.clone(token.value)
            case 2:
                if token.type != .T_OPEN_PARANTHESIS {
                    log_error_with_token(token^, "Expected \'(\' after function identifier")
                    return false, node
                }
            case 3:
                if token.type != .T_VOID_KEYWORD {
                    log_error_with_token(token^, "Function parameters have to be void for now" )
                    return false, node
                }
                function_t.params = strings.clone(token.value)
            case 4:
                if token.type != .T_CLOSE_PARANTHESIS {
                    log_error_with_token(token^, "Expected \')\' after function parameter")
                    return false, node
                }
            case 5:
                if token.type != .T_OPEN_BRACE {
                    log_error_with_token(token^, "Expected \'{\' after function parameters")
                    return false, node
                }
            case 6..<len(iter.tokens):
                if token.type != .T_CLOSE_BRACE {
                    ok, statement_node := resolve_statement(node, iter)
                    if !ok {
                        return false, node
                    }
                    append_ast_node(node, statement_node)
                } else {
                    return true, node
                }
        }

    }

    return false, node
}

@(private="package")
build_ast :: proc(tokens: []Token) -> (bool, ^AstNode) {
	root := new(AstNode)

    iter: TokenIter
    iter.tokens = tokens
    token := &iter.tokens[0]
    for iter.i < len(iter.tokens) {
        if token == nil {
            log(.Error, "Ran out of Tokens to build ast with: Aborting")
        }

        ok, node := resolve_function(root, &iter)
        append_ast_node(root, node)
        if !ok {
            log(.Error, "Could not resolve function: Aborting")
            return false, root
        }
        token := next_token(&iter)
    }

    return true, root
}
