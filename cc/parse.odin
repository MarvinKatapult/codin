package cc

import "core:fmt"
import "core:strings"

NodeType :: enum {
    AST_PROGRAM = 0,
    AST_FUNCTION,
    AST_RETURN,
    AST_CONSTANT,
}

AstValue :: union {
    AstFunction,
    AstStatement,
}

AstNode :: struct {
    childs: [dynamic]AstNode,
    type: NodeType,
    value: AstValue,
}

AstFunction :: struct {
    return_type: AstDataType,
    identifier: string,
    params: string,
    body: [dynamic]AstStatement,
}

AstDataType :: enum {
    Int,
    Void,
}

AstStatement :: struct {
    ret_value: string,
}

cleanup_ast_function :: proc(function_t: AstFunction) {
    delete(function_t.body);
    delete(function_t.identifier);
    delete(function_t.params);
}

cleanup_ast_statement :: proc(statement_t: AstStatement) {
    delete(statement_t.ret_value);
}

cleanup_ast_node :: proc(root: ^AstNode) {
    for &child in root.childs {
        log(.Debug, "Deleting Child");
        cleanup_ast_node(&child);
    }
    switch v in root.value {
        case AstFunction:
            cleanup_ast_function(v);
        case AstStatement:
            cleanup_ast_statement(v);
    }
    if (root.childs != nil) do delete(root.childs);
}

resolve_statement :: proc(root: ^AstNode, tokens: []Token, cursor: ^int) -> (bool, AstStatement) {
    statement_t: AstStatement;

    for cursor in 0..<len(tokens) {
        token := tokens[cursor];
        switch cursor {
            case 0:
                if token.type != .T_RETURN_KEYWORD {
                    log_error_with_token(token, "Statement has to begin with return keyword");
                    return false, statement_t;
                }
            case 1:
                if token.type != .T_INT_LITERAL {
                    log_error_with_token(token, "Token is not valid return value");
                    return false, statement_t;
                }
                statement_t.ret_value = strings.clone(token.value);
            case 2:
                if token.type != .T_SEMICOLON {
                    log_error_with_token(token, "Statement has to end with semicolon");
                    return false, statement_t;
                }
                return true, statement_t;
        }
    }

    return false, statement_t;
}

resolve_function :: proc(root: ^AstNode, tokens: []Token, cursor: ^int) -> (bool, AstNode) {

    node: AstNode;
    node.type = .AST_FUNCTION;

    function_t: AstFunction;
    
    i: int = cursor^;
    for i in 0..<len(tokens) {
        cursor^ = i;
        token := tokens[i];
        switch i {
            case 0:
                if token.type != .T_INT_KEYWORD {
                    log_error_with_token(token, "Token is not a valid return type");
                    return false, node;
                }
                function_t.return_type = .Int;

            case 1:
                if token.type != .T_IDENTIFIER {
                    log_error_with_token(token, "Token is not a valid identifier");
                    return false, node;
                }
                function_t.identifier = strings.clone(token.value);
            case 2:
                if token.type != .T_OPEN_PARANTHESIS {
                    log_error_with_token(token, "Expected \'(\' after function identifier");
                    return false, node;
                }
            case 3:
                if token.type != .T_VOID_KEYWORD {
                    function_t.params = token.value;
                    log_error_with_token(token, "Function parameters have to be void for now" );
                    return false, node;
                }
            case 4:
                if token.type != .T_CLOSE_PARANTHESIS {
                    log_error_with_token(token, "Expected \')\' after function parameter");
                    return false, node;
                }
            case 5:
                if token.type != .T_OPEN_BRACE {
                    log_error_with_token(token, "Expected \'{\' after function parameters");
                    return false, node;
                }
            case 6..<len(tokens):
                if token.type != .T_CLOSE_BRACE {
                    ok, statement_t := resolve_statement(&node, tokens[:], cursor);
                    if !ok {
                        return false, node;
                    }
                    i = cursor^;
                    append(&function_t.body, statement_t);
                } else {
                    node.value = function_t;
                    return true, node;
                }
        }

    }

    return false, node;
}

build_ast :: proc(tokens: []Token) -> (bool, AstNode) {
    root: AstNode;

    i: int = 0;
    for i in 0..<len(tokens) {
        token := tokens[i];
        ok, node := resolve_function(&root, tokens[:], &i);
        append(&root.childs, node);
        if !ok {
            log(.Error, "Could not resolve function: Aborting");
            return false, root;
        }
    }

    return true, root;
}
