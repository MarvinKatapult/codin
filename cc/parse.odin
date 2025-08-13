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
}

append_ast_node :: proc(parent: ^AstNode, child: ^AstNode) {
	append(&parent.childs, child);
	child.parent = parent;
}

@(private="file")
next_token :: proc(iter: ^TokenIter) -> ^Token {
    iter.i += 1;
    if iter.i >= len(iter.tokens) {
        return nil;
    }
    return &iter.tokens[iter.i];
}

@(private="package")
cleanup_ast_function :: proc(function_t: AstFunction) {
    log(.Debug, "Cleaning AstFunction")
    delete(function_t.identifier);
    delete(function_t.params);
}

@(private="package")
cleanup_ast_statement :: proc(statement_t: AstStatement) {
    log(.Debug, "Cleaning AstStatement")
	delete(statement_t.return_value);
}

@(private="package")
cleanup_ast_expression :: proc(expression_t: AstExpression) {
    log(.Debug, "Cleaning AstExpression")
    delete(expression_t.value);
}

@(private="package")
cleanup_ast_node :: proc(root: ^AstNode) {
    for &child in root.childs {
        log(.Debug, "Deleting Child of type: ", fmt.tprintf("%s", child.type));
        cleanup_ast_node(child);
    }
    switch v in root.value {
        case AstFunction:
            cleanup_ast_function(v);
        case AstStatement:
            cleanup_ast_statement(v);
        case AstExpression:
            cleanup_ast_expression(v);
    }
    if (root.childs != nil) do delete(root.childs);
	free(root);
}

@(private="file")
get_operator_for_token :: proc(token: ^Token) -> (bool, Operator) {
	#partial switch token.type {
		case .T_MINUS:			return true, .OP_UNARY_MINUS;
		case .T_TILDE:			return true, .OP_BIT_NEGATION;
		case .T_EXCLAMATION:	return true, .OP_LOGICAL_NOT;
		case: return false, nil;
	}
}

@(private="package")
resolve_expression :: proc(root: ^AstNode, iter: ^TokenIter) -> (bool, ^AstNode) {
	node := new(AstNode);
    expression_t: AstExpression;
	node.value = expression_t;
	
	token := &iter.tokens[iter.i];

	if token == nil {
		log(.Error, "Ran out of tokens whilst resolving expression: Aborting");
		return false, node;
	}

	if token.type == .T_INT_LITERAL {
		node.type = .AST_EXPRESSION_CONSTANT;
		expression_t.value = strings.clone(token.value);
	} else {
		ok: bool;
		ok, expression_t.operator = get_operator_for_token(token);
		if !ok {
			log_error_with_token(token^, "is not a valid Unary Operator!");
			return false, node;
		}
		
		next_token(iter);
		child_expression: ^AstNode;
		ok, child_expression = resolve_expression(node, iter);
		if !ok {
			return false, node;
		}
		node.type = .AST_EXPRESSION_UNARY;
    }
	node.value = expression_t;
	append_ast_node(root, node);
	return true, node;
}

@(private="package")
resolve_statement :: proc(root: ^AstNode, iter: ^TokenIter) -> (bool, ^AstNode) {
	node := new(AstNode);
    statement_t: AstStatement;

    token: ^Token;
    for i := 0; ; i += 1 {
        node.value = statement_t;

        if i != 0 do token = next_token(iter);
        else do token = &iter.tokens[iter.i];

        if token == nil {
            log(.Error, "Ran out of tokens whilst resolving statement: Aborting");
            return false, node;
        }

        buf: [8]u8;
        strconv.itoa(buf[:], i)
        switch i {
            case 0:
                if token.type != .T_RETURN_KEYWORD {
                    log_error_with_token(token^, "Statement has to begin with return keyword");
					cleanup_ast_node(node);
                    return false, node;
                }
                node.type = .AST_RETURN_STATEMENT;
            case 1:
                ok, node_expression := resolve_expression(node, iter)
                if !ok {
					cleanup_ast_node(node);
                    return false, node;
                }
				statement_t.return_value = node_expression.value.(AstExpression).value;

                next_token(iter);
                token = &iter.tokens[iter.i];
                if token.type != .T_SEMICOLON {
                    log_error_with_token(token^, "Statement has to end with ;");
					cleanup_ast_node(node);
                    return false, node;
                }
                
                return true, node;
        }
    }

    return false, node;
}

@(private="package")
resolve_function :: proc(root: ^AstNode, iter: ^TokenIter) -> (bool, ^AstNode) {

	node := new(AstNode);
    node.type = .AST_FUNCTION;

    function_t: AstFunction;
    
    for i := 0; ; i += 1 {
        token: ^Token;
        node.value = function_t;

        buf: [8]u8;
        strconv.itoa(buf[:], i)

        if i != 0 do token = next_token(iter);
        else do token = &iter.tokens[iter.i];

        if token == nil {
            log(.Error, "Ran out of tokens whilst resolving function: Aborting!");
            return false, node;
        }

        switch i {
            case 0:
                if token.type != .T_INT_KEYWORD {
                    log_error_with_token(token^, "Token is not a valid return type");
                    return false, node;
                }
                function_t.ret_type = .Int;

            case 1:
                if token.type != .T_IDENTIFIER {
                    log_error_with_token(token^, "Token is not a valid identifier");
                    return false, node;
                }
                function_t.identifier = strings.clone(token.value);
            case 2:
                if token.type != .T_OPEN_PARANTHESIS {
                    log_error_with_token(token^, "Expected \'(\' after function identifier");
                    return false, node;
                }
            case 3:
                if token.type != .T_VOID_KEYWORD {
                    log_error_with_token(token^, "Function parameters have to be void for now" );
                    return false, node;
                }
                function_t.params = strings.clone(token.value);
            case 4:
                if token.type != .T_CLOSE_PARANTHESIS {
                    log_error_with_token(token^, "Expected \')\' after function parameter");
                    return false, node;
                }
            case 5:
                if token.type != .T_OPEN_BRACE {
                    log_error_with_token(token^, "Expected \'{\' after function parameters");
                    return false, node;
                }
            case 6..<len(iter.tokens):
                if token.type != .T_CLOSE_BRACE {
                    ok, statement_node := resolve_statement(node, iter);
                    if !ok {
                        return false, node;
                    }
                    append_ast_node(node, statement_node);
                } else {
                    return true, node;
                }
        }

    }

    return false, node;
}

@(private="package")
build_ast :: proc(tokens: []Token) -> (bool, ^AstNode) {
	root := new(AstNode);

    iter: TokenIter;
    iter.tokens = tokens;
    token := &iter.tokens[0];
    for iter.i < len(iter.tokens) {
        if token == nil {
            log(.Error, "Ran out of Tokens to build ast with: Aborting");
        }

        ok, node := resolve_function(root, &iter);
        append_ast_node(root, node);
        if !ok {
            log(.Error, "Could not resolve function: Aborting");
            return false, root;
        }
        token := next_token(&iter);
    }

    return true, root;
}
