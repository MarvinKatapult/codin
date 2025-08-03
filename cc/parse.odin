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
    AST_STATEMENT,
}

@(private="package")
AstValue :: union {
    AstFunction,
    AstStatement,
}

@(private="package")
AstNode :: struct {
    childs: [dynamic]AstNode,
    type: NodeType,
    value: AstValue,
}

@(private="package")
AstFunction :: struct {
    return_type: AstDataType,
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
    ret_value: string,
}

@(private="file")
TokenIter :: struct {
    tokens: []Token,
    i: int,
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
    delete(statement_t.ret_value);
}

@(private="package")
cleanup_ast_node :: proc(root: ^AstNode) {
    for &child in root.childs {
        log(.Debug, "Deleting Child of type: ", fmt.tprintf("%s", child.type));
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

@(private="package")
resolve_statement :: proc(root: ^AstNode, iter: ^TokenIter) -> (bool, AstNode) {
    node: AstNode;
    node.type = .AST_STATEMENT;
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
        log(.Debug, "i in resolve_statement = ", string(buf[:]));
        switch i {
            case 0:
                if token.type != .T_RETURN_KEYWORD {
                    log_error_with_token(token^, "Statement has to begin with return keyword");
                    return false, node;
                }
            case 1:
                if token.type != .T_INT_LITERAL {
                    log_error_with_token(token^, "Token is not valid return value");
                    return false, node;
                }
                statement_t.ret_value = strings.clone(token.value);
            case 2:
                if token.type != .T_SEMICOLON {
                    log_error_with_token(token^, "Statement has to end with semicolon");
                    return false, node;
                }
                return true, node;
        }
    }

    return false, node;
}

@(private="package")
resolve_function :: proc(root: ^AstNode, iter: ^TokenIter) -> (bool, AstNode) {

    node: AstNode;
    node.type = .AST_FUNCTION;

    function_t: AstFunction;
    
    for i := 0; ; i += 1 {
        token: ^Token;
        node.value = function_t;

        buf: [8]u8;
        strconv.itoa(buf[:], i)
        log(.Debug, "i in resolve_function = ", string(buf[:]));

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
                function_t.return_type = .Int;

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
                    function_t.params = token.value;
                    log_error_with_token(token^, "Function parameters have to be void for now" );
                    return false, node;
                }
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
                    ok, statement_node := resolve_statement(&node, iter);
                    if !ok {
                        return false, node;
                    }
                    append(&node.childs, statement_node);
                } else {
                    return true, node;
                }
        }

    }

    return false, node;
}

@(private="package")
build_ast :: proc(tokens: []Token) -> (bool, AstNode) {
    root: AstNode;

    iter: TokenIter;
    iter.tokens = tokens;
    token := &iter.tokens[0];
    for iter.i < len(iter.tokens) {
        if token == nil {
            log(.Error, "Ran out of Tokens to build ast with: Aborting");
        }

        ok, node := resolve_function(&root, &iter);
        append(&root.childs, node);
        if !ok {
            log(.Error, "Could not resolve function: Aborting");
            return false, root;
        }
        token := next_token(&iter);
    }

    return true, root;
}
