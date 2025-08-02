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

}

cleanup_ast_function :: proc(function_t: AstFunction) {
    delete(function_t.body);
    delete(function_t.identifier);
    delete(function_t.params);
}

cleanup_ast_statement :: proc(statement_t: AstStatement) {

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

resolve_function :: proc(root: ^AstNode, tokens: []Token, i: int) -> (AstNode, bool) {

    node: AstNode;
    node.type = .AST_FUNCTION;

    function_t: AstFunction;
    
    for token, i in tokens {
        switch i {
            case 0:
                if token.type != .T_INT_KEYWORD {
                    log_error_with_token(token, "Token is not a valid return type");
                    return node, false;
                }
                function_t.return_type = .Int;

            case 1:
                if token.type != .T_IDENTIFIER {
                    log_error_with_token(token, "Token is not a valid identifier");
                    return node, false;
                }
                function_t.identifier = strings.clone(token.value);
        }

    }
    // Muss nacher in case }
    node.value = function_t;
    // ---

    return node, false;
}

build_ast :: proc(tokens: []Token) -> (bool, AstNode) {
    root: AstNode;

    for token, i in tokens {
        node, ok := resolve_function(&root, tokens, i);
        append(&root.childs, node);
        if !ok {
            log(.Error, "Could not resolve function: Aborting");
            return false, root;
        }
    }

    return true, root;
}
