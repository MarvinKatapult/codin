package cc

import "core:os"
import "core:fmt"
import "core:strings"

@(private="package")
NodeType :: enum {
    AST_NOTHING = 0,
    AST_PROGRAM,
    AST_SCOPE,
    AST_FUNCTION,
    AST_FUNCTION_DECLARE,
    AST_RETURN,
    AST_CONSTANT,
    AST_RETURN_STATEMENT,
    AST_VAR_DECLARE,
    AST_VAR_ASSIGNMENT,
    AST_VAR_DEREF_ASSIGNMENT,
    AST_VARIADIC_ARGS,
    AST_EXPR_STATEMENT,
    AST_EXPR_CONSTANT,
    AST_EXPR_UNARY,
    AST_EXPR_BINARY,
    AST_EXPR_VARIABLE,
    AST_FUNC_CALL,
    AST_IF,
    AST_ELSE,
    AST_ELSE_IF,
    AST_WHILE,
    AST_DO_WHILE,
    AST_FOR,
    AST_BREAK,
    AST_STRING_LITERAL,
    AST_STRUCT_DECLARE,
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
    AstScope,
}

@(private="package")
AstFunction :: struct {
    ret_type:   DataType,
    identifier: string,
}

@(private="package")
AstStatement :: struct {
    identifier: string,
    value:      string,
    type:        DataType,
}

@(private="package")
AstExpression :: struct {
    value:    string,
    type:     DataType,
    operator: Operator,
}

@(private="package")
AstScope :: struct { }

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
    OP_BIT_OR,
    OP_BIT_AND,
    OP_BIT_XOR,
    OP_BIT_SHL,
    OP_BIT_SHR,
    OP_BINARY_PLUS,
    OP_BINARY_MINUS,
    OP_BINARY_MULT,
    OP_BINARY_DIV,
    OP_BINARY_MOD,
    OP_BINARY_LESS,
    OP_BINARY_LESS_EQUAL,
    OP_BINARY_GREATER,
    OP_BINARY_GREATER_EQUAL,
    OP_BINARY_EQUAL,
    OP_BINARY_NOT_EQUAL,
    OP_LOGICAL_OR,
    OP_LOGICAL_AND,
    OP_ADRESS,
    OP_DEREFERENCE,
}

@(private="package")
DataType :: struct {
    subtypes:  [dynamic]DataType,
    name:      string,
    size:      int,
    is_float:  bool,
    unsigned:  bool,
    is_struct: bool,
    variadic:  bool,
}

@(private="package")
ParseInfo :: struct {
    functions:             [dynamic]AstFunction,
    types:                 [dynamic]DataType,
    program_node:          ^AstNode,
    global_count:          uint,
    break_possible:        bool,
    no_semicolon:          bool,
    no_declare_and_assign: bool,
}

@(private="package")
append_ast_node :: proc(parent: ^AstNode, child: ^AstNode) {
    append(&parent.childs, child)
    child.parent = parent
}

@(private="package")
prepend_ast_node :: proc(parent: ^AstNode, child: ^AstNode) {
    inject_at(&parent.childs, 0, child)
    child.parent = parent
}

@(private="file")
next_token :: proc(iter: ^TokenIter, place := #caller_location, fail := true) -> ^Token {
    iter.i += 1
    if iter.i >= len(iter.tokens) {
        if fail {
            log(.Error, "Unexpected end of tokens", place = place)
            os.exit(-1)
        }
        return nil
    }
    return &iter.tokens[iter.i]
}

@(private="file")
look_ahead_token :: proc(iter: ^TokenIter, place := #caller_location, fail := true) -> ^Token {
    if iter.i + 1 >= len(iter.tokens) {
        if fail {
            log(.Error, "Ran out of token", place = place)
            os.exit(-1)
        }
        return nil
    }
    return &iter.tokens[iter.i+1]
}

@(private="package")
current_token :: proc(iter: ^TokenIter) -> ^Token {
    if iter.i >= len(iter.tokens) {
        return nil
    }
    return &iter.tokens[iter.i]
}

@(private="file")
token_left :: proc(iter: ^TokenIter, place := #caller_location) -> bool {
    if current_token(iter) == nil {
        log(.Error, "Ran out of tokens whilst resolving statement: Aborting", place = place)
        return false
    }
    return true
}

@(private="package")
prev_token :: proc(iter: ^TokenIter) -> ^Token {
    if iter.i - 1 < 0 do panic("prev_token called with first token!")
    return &iter.tokens[iter.i - 1]
}

@(private="file")
get_operator_for_token :: proc(token: ^Token, unary: bool) -> (Operator, bool) {
    #partial switch token.type {
        case .T_MINUS:            return unary ? .OP_UNARY_MINUS : .OP_BINARY_MINUS,     true
        case .T_AMPERSAND:        return unary ? .OP_ADRESS      : .OP_BIT_AND,         true
        case .T_STAR:            return unary ? .OP_DEREFERENCE : .OP_BINARY_MULT,    true
        case .T_PLUS:            return .OP_BINARY_PLUS,             true
        case .T_FSLASH:            return .OP_BINARY_DIV,             true
        case .T_PERCENT:        return .OP_BINARY_MOD,             true
        case .T_TILDE:            return .OP_BIT_NEGATION,         true
        case .T_EXCLAMATION:    return .OP_LOGICAL_NOT,             true
        case .T_GREATER:        return .OP_BINARY_GREATER,         true
        case .T_GREATER_EQUAL:    return .OP_BINARY_GREATER_EQUAL, true
        case .T_LESS:            return .OP_BINARY_LESS,             true
        case .T_LESS_EQUAL:        return .OP_BINARY_LESS_EQUAL,     true
        case .T_EQUAL_EQUAL:    return .OP_BINARY_EQUAL,         true
        case .T_NOT_EQUAL:        return .OP_BINARY_NOT_EQUAL,     true
        case .T_LOGICAL_OR:        return .OP_LOGICAL_OR,             true
        case .T_LOGICAL_AND:    return .OP_LOGICAL_AND,             true
        case .T_BIT_XOR:        return .OP_BIT_XOR,                 true
        case .T_BIT_OR:            return .OP_BIT_OR,                 true
        case .T_SHIFT_LEFT:        return .OP_BIT_SHL,                 true
        case .T_SHIFT_RIGHT:    return .OP_BIT_SHR,                 true
        case: return nil, false
    }
}

@(private="file")
is_token_unary_operator :: proc(token: ^Token) -> bool {
    #partial switch token.type {
        case .T_EXCLAMATION : fallthrough
        case .T_TILDE        : fallthrough
        case .T_AMPERSAND        : fallthrough
        case .T_STAR        : fallthrough
        case .T_MINUS        :
            return true
    }
    return false
}

@(private="package")
get_terminated_char_value :: proc(c: u8) -> (val: u8, ok: bool) {
    switch c {
        case 'n':
            return '\n', true
        case 't':
            return '\t', true
        case '\"':
            return '\"', true
        case '\'':
            return '\'', true
        case '\\':
            return '\\', true
        case '\r':
            return '\r', true
        case '\v':
            return '\v', true
        case '0':
            return 0, true
    }
    return 0, false
}

@(private="file")
valid_single_quote_tokens :: proc(iter: ^TokenIter, val: ^u8) -> bool {

    token := current_token(iter).value
    if len(token) != 1 do return false

    if token == "\\" {
        next_token(iter)
        terminated_char := current_token(iter).value
        if len(terminated_char) != 1 do return false

        ok: bool
        val^, ok = get_terminated_char_value(terminated_char[0])

        return ok
    }

    val^ = current_token(iter).value[0]
    return true
}

@(private="file")
get_integer_literal_value :: proc(iter: ^TokenIter) -> (val: string , ok: bool) {
    if current_token(iter).type == .T_SINGLE_QUOTE {
        next_token(iter)

        char: u8
        if !valid_single_quote_tokens(iter, &char) {
            log_error_with_token(current_token(iter)^, "Single quotes only allow one character")
            return "", false
        }

        val = strings.clone(fmt.tprintf("%d", char))

        next_token(iter)
        if current_token(iter).type != .T_SINGLE_QUOTE {
            log_error_with_token(current_token(iter)^, "Expected closing \"\'\" after character")
            return "", false
        }
        
        return val, true
    }

    if current_token(iter).type == .T_INT_LITERAL {
        val = current_token(iter).value
        return val, true
    }

    return val, false
}

@(private="file")
resolve_expr_primary :: proc(iter: ^TokenIter, parse_info: ^ParseInfo, no_expr_possible := true) -> (node: ^AstNode, ok: bool) {
    
    node = new(AstNode)
    token_left(iter) or_return

    // ()
    if current_token(iter).type == .T_OPEN_PARANTHESIS {
        next_token(iter)
        node = resolve_expr(nil, iter, parse_info) or_return

        if current_token(iter).type != .T_CLOSE_PARANTHESIS {
            log(.Error, "Expected ')' but found:", fmt.tprintf("%s", current_token(iter).type))
            return node, false
        }

        next_token(iter)
        return node, true
    }

    if current_token(iter).type == .T_DOUBLE_QUOTE {
        node = resolve_string_literals(iter, parse_info) or_return
        return node, true
    }

    // 3, 4, 7
    number, is_number := get_integer_literal_value(iter)
    if is_number {
        expression_t: AstExpression
        expression_t.value = number
        node.value = expression_t
        node.type = .AST_EXPR_CONSTANT
        
        next_token(iter)
        return node, true
    }

    // foo (Either function or variable)
    if current_token(iter).type == .T_IDENTIFIER {
        expression_t: AstExpression
        expression_t.value = strings.clone(current_token(iter).value)
        node.value = expression_t

        is_function_call := look_ahead_token(iter).type == .T_OPEN_PARANTHESIS 
        if is_function_call {
            node.type = .AST_FUNC_CALL

            next_token(iter)
            next_token(iter)

            first_param := true
            for current_token(iter).type != .T_CLOSE_PARANTHESIS {
                if !first_param && current_token(iter).type != .T_COMMA {
                    log_error_with_token(current_token(iter)^, "Expected ','")
                    return node, false
                }
                if !first_param do next_token(iter)

                first_param = false

                expr := resolve_expr(node, iter, parse_info, no_expr_possible = false) or_return
                append_ast_node(node, expr)
            }
        } else {
            node.type = .AST_EXPR_VARIABLE
        }

        next_token(iter)
        return node, true
    }
    
    // ! ~ -
    if is_token_unary_operator(current_token(iter)) {
        expression_t: AstExpression
        expression_t.operator = get_operator_for_token(current_token(iter), true) or_return

        node.value = expression_t
        node.type = .AST_EXPR_UNARY

        next_token(iter)
        expr := resolve_expr_primary(iter, parse_info, no_expr_possible = false) or_return
        append_ast_node(node, expr)
        return node, true
    }

    if no_expr_possible && current_token(iter).type == .T_SEMICOLON {
        node.type = .AST_NOTHING
        return node, true
    }

    log_error_with_token(current_token(iter)^, "Could not resolve primary expression")

    return node, false
}

@(private="file")
resolve_expr_dot :: proc(parent: ^AstNode, iter: ^TokenIter, no_expr_possible := true, parse_info: ^ParseInfo) -> (left_child: ^AstNode, ok: bool) {

    left_child, ok = resolve_expr_primary(iter, parse_info, no_expr_possible)
    if !ok {
        return left_child, false
    }

    token_left(iter) or_return 

    // * /
    for current_token(iter).type == .T_STAR || current_token(iter).type == .T_FSLASH || current_token(iter).type == .T_PERCENT {
        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        expression_t.operator = get_operator_for_token(op_token, false) or_return
        
        right_child: ^AstNode
        right_child, ok = resolve_expr_primary(iter, parse_info, no_expr_possible)
        if !ok {
            return left_child, false
        }
        
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
resolve_expr_additive :: proc(parent: ^AstNode, iter: ^TokenIter, no_expr_possible := true, parse_info: ^ParseInfo) -> (left_child: ^AstNode, ok: bool) {
    
    left_child, ok = resolve_expr_dot(parent, iter, no_expr_possible, parse_info)
    if !ok {
        return left_child, ok
    }
    
    // + -
    for current_token(iter).type == .T_PLUS || current_token(iter).type == .T_MINUS {
        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        expression_t.operator = get_operator_for_token(op_token, false) or_return
        
        right_child := resolve_expr_dot(nil, iter, no_expr_possible, parse_info) or_return
        
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
resolve_expr_bit_shift :: proc(parent: ^AstNode, iter: ^TokenIter, no_expr_possible := true, parse_info: ^ParseInfo) -> (left_child: ^AstNode, ok: bool) {
    
    left_child, ok = resolve_expr_additive(parent, iter, no_expr_possible, parse_info)
    if !ok {
        return left_child, ok
    }
    
    // + -
    for current_token(iter).type == .T_SHIFT_LEFT || current_token(iter).type == .T_SHIFT_RIGHT {
        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        expression_t.operator = get_operator_for_token(op_token, false) or_return
        
        right_child := resolve_expr_additive(nil, iter, no_expr_possible, parse_info) or_return
        
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
resolve_expr_comparing :: proc(parent: ^AstNode, iter: ^TokenIter, no_expr_possible := true, parse_info: ^ParseInfo) -> (left_child: ^AstNode, ok: bool) {
    
    left_child, ok = resolve_expr_bit_shift(parent, iter, no_expr_possible, parse_info)
    if !ok {
        return left_child, ok
    }
    
    // < <= >= >
    for current_token(iter).type == .T_GREATER || 
        current_token(iter).type == .T_LESS || 
        current_token(iter).type == .T_LESS_EQUAL || 
        current_token(iter).type == .T_GREATER_EQUAL {

        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        expression_t.operator = get_operator_for_token(op_token, false) or_return
        
        right_child := resolve_expr_bit_shift(nil, iter, no_expr_possible, parse_info) or_return
        
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
resolve_expr_equal :: proc(parent: ^AstNode, iter: ^TokenIter, no_expr_possible := true, parse_info: ^ParseInfo) -> (left_child: ^AstNode, ok: bool) {
    
    left_child, ok = resolve_expr_comparing(parent, iter, no_expr_possible, parse_info)
    if !ok {
        return left_child, ok
    }
    
    // == !=
    for current_token(iter).type == .T_EQUAL_EQUAL || 
        current_token(iter).type == .T_NOT_EQUAL {

        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        expression_t.operator = get_operator_for_token(op_token, false) or_return
        
        right_child := resolve_expr_comparing(nil, iter, no_expr_possible, parse_info) or_return
        
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
resolve_expr_bit_xor :: proc(parent: ^AstNode, iter: ^TokenIter, no_expr_possible := true, parse_info: ^ParseInfo) -> (left_child: ^AstNode, ok: bool) {
    
    left_child, ok = resolve_expr_equal(parent, iter, no_expr_possible, parse_info)
    if !ok {
        return left_child, ok
    }
    
    // |
    for current_token(iter).type == .T_BIT_XOR {

        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        expression_t.operator = get_operator_for_token(op_token, false) or_return
        
        right_child := resolve_expr_equal(nil, iter, no_expr_possible, parse_info) or_return
        
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
resolve_expr_bit_and :: proc(parent: ^AstNode, iter: ^TokenIter, no_expr_possible := true, parse_info: ^ParseInfo) -> (left_child: ^AstNode, ok: bool) {
    
    left_child, ok = resolve_expr_bit_xor(parent, iter, no_expr_possible, parse_info)
    if !ok {
        return left_child, ok
    }
    
    // |
    for current_token(iter).type == .T_AMPERSAND {

        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        expression_t.operator = get_operator_for_token(op_token, false) or_return
        
        right_child := resolve_expr_bit_xor(nil, iter, no_expr_possible, parse_info) or_return
        
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
resolve_expr_bit_or :: proc(parent: ^AstNode, iter: ^TokenIter, no_expr_possible := true, parse_info: ^ParseInfo) -> (left_child: ^AstNode, ok: bool) {
    
    left_child, ok = resolve_expr_bit_and(parent, iter, no_expr_possible, parse_info)
    if !ok {
        return left_child, ok
    }
    
    // |
    for current_token(iter).type == .T_BIT_OR {

        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        expression_t.operator = get_operator_for_token(op_token, false) or_return
        
        right_child := resolve_expr_bit_and(nil, iter, no_expr_possible, parse_info) or_return
        
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
resolve_expr_log_and :: proc(parent: ^AstNode, iter: ^TokenIter, no_expr_possible := true, parse_info: ^ParseInfo) -> (left_child: ^AstNode, ok: bool) {
    
    left_child, ok = resolve_expr_bit_or(parent, iter, no_expr_possible, parse_info)
    if !ok {
        return left_child, ok
    }
    
    // &&
    for current_token(iter).type == .T_LOGICAL_AND {

        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        expression_t.operator = get_operator_for_token(op_token, false) or_return
        
        right_child := resolve_expr_bit_or(nil, iter, no_expr_possible, parse_info) or_return
        
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
resolve_expr_log_or :: proc(parent: ^AstNode, iter: ^TokenIter, no_expr_possible := true, parse_info: ^ParseInfo) -> (left_child: ^AstNode, ok: bool) {
    
    left_child, ok = resolve_expr_log_and(parent, iter, no_expr_possible, parse_info)
    if !ok {
        return left_child, ok
    }
    
    // ||
    for current_token(iter).type == .T_LOGICAL_OR {

        op_token := current_token(iter)
        next_token(iter)
        
        expression_t: AstExpression
        expression_t.operator = get_operator_for_token(op_token, false) or_return
        
        right_child := resolve_expr_log_and(nil, iter, no_expr_possible, parse_info) or_return
        
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
resolve_expr :: proc(root: ^AstNode, iter: ^TokenIter, parse_info: ^ParseInfo, no_expr_possible := true) -> (^AstNode, bool) {
    return resolve_expr_log_or(root, iter, no_expr_possible, parse_info)
}

@(private="package")
token_assignment_operator :: proc(token: Token, op: ^Operator, only_equal_assign: bool) -> bool {

    if only_equal_assign {
        if op != nil do op^ = .OP
        return token.type == .T_ASSIGNMENT
    }

    #partial switch token.type {
        case .T_ASSIGN_PLUS:
            if op != nil do op^ = .OP_BINARY_PLUS
        case .T_ASSIGN_MINUS:
            if op != nil do op^ = .OP_BINARY_MINUS
        case .T_ASSIGN_MULT:
            if op != nil do op^ = .OP_BINARY_MULT
        case .T_ASSIGN_DIV:
            if op != nil do op^ = .OP_BINARY_DIV
        case .T_ASSIGN_PERCENT:
            if op != nil do op^ = .OP_BINARY_MOD
        case .T_ASSIGN_SHIFTL:
            if op != nil do op^ = .OP_BIT_SHL
        case .T_ASSIGN_SHIFTR:
            if op != nil do op^ = .OP_BIT_SHR
        case .T_ASSIGN_AND:
            if op != nil do op^ = .OP_BIT_AND
        case .T_ASSIGN_XOR:
            if op != nil do op^ = .OP_BIT_XOR
        case .T_ASSIGN_OR:
            if op != nil do op^ = .OP_BIT_OR
        case .T_ASSIGNMENT:
            if op != nil do op^ = .OP

        case:
            return false
    }
    return true
}

@(private="file")
resolve_string_literals :: proc(iter: ^TokenIter, parse_info: ^ParseInfo) -> (node: ^AstNode, ok: bool) {
    node = new(AstNode)
    node.type = .AST_STRING_LITERAL

    if current_token(iter).type != .T_DOUBLE_QUOTE {
        return node, false
    }
    next_token(iter)

    statement_t: AstStatement
    str_b := strings.builder_make()
    for current_token(iter).type == .T_STRING_LITERAL {
        strings.write_string(&str_b, current_token(iter).value)
        next_token(iter)
    }

    if current_token(iter).type != .T_DOUBLE_QUOTE {
        return node, false
    }
    next_token(iter)

    statement_t.value      = strings.clone(strings.to_string(str_b))
    statement_t.identifier = strings.clone(fmt.tprintf("GLB_STR_%d", parse_info.global_count))
    parse_info.global_count += 1

    node.value = statement_t
    prepend_ast_node(parse_info.program_node, node)

    node = new(AstNode)
    node.type = .AST_STRING_LITERAL

    expression_t: AstExpression
    expression_t.value = statement_t.identifier
    node.value = expression_t

    return node, true
}

@(private="package")
resolve_assignment :: proc(root: ^AstNode, iter: ^TokenIter, parse_info: ^ParseInfo, only_equal_assign := true) -> bool {
    statement_t: AstStatement
    statement_t.identifier = strings.clone(prev_token(iter).value)
    root.value = statement_t
    
    op: Operator
    if !token_assignment_operator(current_token(iter)^, &op, only_equal_assign) {
        log_error_with_token(current_token(iter)^, "Expected assignment operator")
        return false
    }

    next_token(iter)

    ok: bool
    expr_node: ^AstNode
    expr_node, ok = resolve_expr(root, iter, parse_info, no_expr_possible = false)

    if !ok {
        log(.Error, "Could not resolve expression")
        return false
    }

    if op != .OP {
        expression_t: AstExpression
        expression_t.operator = op
        
        binary_node := new(AstNode)
        binary_node.type = .AST_EXPR_BINARY
        binary_node.value = expression_t
        append_ast_node(root, binary_node)

        expression_t.value = root.value.(AstStatement).identifier
    
        var_node := new(AstNode)
        var_node.type = .AST_EXPR_VARIABLE
        var_node.value = expression_t

        append_ast_node(binary_node, var_node)
        append_ast_node(binary_node, expr_node)

    // Just an assignment
    } else {
        append_ast_node(root, expr_node)
    }

    return true
}

@(private="file")
get_token_type_info :: proc(iter: ^TokenIter, parse_info: ^ParseInfo, type_info: ^DataType) -> bool {
    unsigned  := false
    is_struct := false

    if current_token(iter).type == .T_UNSIGNED {
        unsigned = true
        next_token(iter)
    } else if current_token(iter).type == .T_SIGNED {
        next_token(iter)
    } else if current_token(iter).type == .T_STRUCT {
        is_struct = true
    }

    if current_token(iter).type != .T_IDENTIFIER && !is_struct {
        return false
    }

    identifier := is_struct ? look_ahead_token(iter).value : current_token(iter).value
    for type in parse_info.types {
        if identifier == type.name {
            type_info^ = type
            type_info.unsigned = unsigned
            if is_struct do next_token(iter)
            return true
        }
    }
    return false
}

@(private="file")
resolve_variable_declaration :: proc(iter: ^TokenIter, parse_info: ^ParseInfo) -> (node: ^AstNode, ok: bool) {
    // Integer Declaration
    node = new(AstNode)
    node.type = .AST_VAR_DECLARE
    statement_t: AstStatement
    node.value = statement_t
    ok = false

    if get_token_type_info(iter, parse_info, &statement_t.type) {
        node.value = statement_t
        log(.Debug, fmt.tprint(statement_t.type))

        next_token(iter)
        if statement_t.type.size <= 0 {
            log_error_with_token(current_token(iter)^, "type is not valid type for variable declaration")
            return node, false
        }

        if current_token(iter).type != .T_IDENTIFIER {
            log_error_with_token(current_token(iter)^, "Expected identifier after Datatype keyword")
            return node, false
        }

        statement_t.identifier = strings.clone(current_token(iter).value)
        node.type = .AST_VAR_DECLARE
        node.value = statement_t

        next_token(iter)

        if current_token(iter).type == .T_ASSIGNMENT {
            assignment_node := new(AstNode)
            assignment_node.type = .AST_VAR_ASSIGNMENT
            append_ast_node(node, assignment_node)

            if parse_info.no_declare_and_assign || !resolve_assignment(assignment_node, iter, parse_info) {
                return node, false
            }
        }

        ok = true
    }
    log(.Debug, fmt.tprint(statement_t.type))

    return node, ok
}

calc_size_of_struct :: proc(node: ^AstNode) -> (size: int) {
    for child in node.childs {
        statement_t := child.value.(AstStatement)
        size += statement_t.type.size
    }
    return size
}

resolve_struct_declaration :: proc(iter: ^TokenIter, parse_info: ^ParseInfo) -> (node: ^AstNode, ok: bool) {

    node = new(AstNode)

    if current_token(iter).type != .T_STRUCT {
        return node, false
    }

    next_token(iter)

    if current_token(iter).type != .T_IDENTIFIER {
        return node, false
    }

    statement_t: AstStatement
    statement_t.identifier = strings.clone(current_token(iter).value)

    node.type = .AST_STRUCT_DECLARE
    node.value = statement_t
    next_token(iter)

    if current_token(iter).type != .T_OPEN_BRACE {
        return node, false
    }

    next_token(iter)

    new_type := DataType {
        is_struct = true,
        name = statement_t.identifier
    }

    for current_token(iter).type != .T_CLOSE_BRACE {

        parse_info.no_declare_and_assign = true
        declaration: ^AstNode
        declaration, ok = resolve_variable_declaration(iter, parse_info)
        if !ok do break
        parse_info.no_declare_and_assign = false

        parsed_type := declaration.value.(AstStatement).type
        new_type.size += parsed_type.size
        append(&new_type.subtypes, parsed_type)

        append_ast_node(node, declaration)

        if current_token(iter).type != .T_SEMICOLON {
            log_error_with_token(current_token(iter)^, "Expected ; or end of struct declaration")
            return node, false
        }

        next_token(iter)
    }

    append(&parse_info.types, new_type)

    if len(node.childs) <= 0 {
        log_error_with_token(current_token(iter)^, "Struct declaration with no fields is not allowed")
        return node, false
    }

    next_token(iter)

    return node, ok
}

@(private="package")
resolve_statement :: proc(iter: ^TokenIter, parse_info: ^ParseInfo) -> (node: ^AstNode, ok: bool) {

    statement_t: AstStatement
    node = new(AstNode)
    node.value = statement_t

    ok = true
    parsed_stmt := false

    if !token_left(iter) {
        return node, false
    }

    if !parsed_stmt && current_token(iter).type == .T_BREAK {
        parsed_stmt = true
        node.type = .AST_BREAK
        if !parse_info.break_possible {
            log_error_with_token(current_token(iter)^, "Break only possible in switch or loop")
            return node, false
        }
        next_token(iter)
    }

    // Inner Scope
    if !parsed_stmt && current_token(iter).type == .T_OPEN_BRACE {
        node, ok = resolve_scope(iter, parse_info)
        return node, ok
    }

    // Return statement
    if !parsed_stmt && current_token(iter).type == .T_RETURN_KEYWORD {
        next_token(iter)

        expr_node: ^AstNode
        expr_node, ok = resolve_expr(node, iter, parse_info)
        append_ast_node(node, expr_node)

        if !ok {
            log_error_with_token(current_token(iter)^, "Return statement does not end with valid expression")
            return node, false
        }

        node.type = .AST_RETURN_STATEMENT
        parsed_stmt = true
    }

    if !parsed_stmt {
        node, parsed_stmt = resolve_variable_declaration(iter, parse_info)
    }

    if !parsed_stmt {
        node, parsed_stmt = resolve_struct_declaration(iter, parse_info)
    }

    // if-statement
    if !parsed_stmt && current_token(iter).type == .T_IF {
        parsed_stmt = true
        node.type = .AST_IF
        next_token(iter)

        if current_token(iter).type != .T_OPEN_PARANTHESIS {
            log_error_with_token(current_token(iter)^, "if-statement condition has to be wrapped in (...)")
            return node, false
        }

        expr := resolve_expr(node, iter, parse_info, no_expr_possible = false) or_return
        append_ast_node(node, expr)

        if_scope := resolve_scope(iter, parse_info, allow_single_stmt = true) or_return
        append_ast_node(node, if_scope)

        for look_ahead_token(iter).type == .T_ELSE {
            next_token(iter) // else

            if look_ahead_token(iter).type == .T_IF {
                next_token(iter) // IF
                next_token(iter) // ()
                else_if_node := new(AstNode)
                else_if_node.type = .AST_ELSE_IF
                
                if current_token(iter).type != .T_OPEN_PARANTHESIS {
                    log_error_with_token(current_token(iter)^, "else-if-statement condition has to be wrapped in (...)")
                    return node, false
                }

                expr = resolve_expr(else_if_node, iter, parse_info, no_expr_possible = false) or_return
                append_ast_node(else_if_node, expr)

                else_if_scope := resolve_scope(iter, parse_info, allow_single_stmt = true) or_return
                append_ast_node(else_if_node, else_if_scope)

                append_ast_node(node, else_if_node)

                continue
            }

            else_node := new(AstNode)
            else_node.type = .AST_ELSE
            append_ast_node(node, else_node)

            next_token(iter)  // {

            else_scope := resolve_scope(iter, parse_info, allow_single_stmt = true) or_return
            append_ast_node(else_node, else_scope)
            break
        }

        return node, ok
    }

    if !parsed_stmt && current_token(iter).type == .T_WHILE {
        parsed_stmt = true
        next_token(iter)

        if current_token(iter).type != .T_OPEN_PARANTHESIS {
            log_error_with_token(current_token(iter)^, "while-loop condition has to be wrapped in (...)")
            return node, false
        }

        node.type = .AST_WHILE

        expr := resolve_expr(node, iter, parse_info, no_expr_possible = false) or_return
        append_ast_node(node, expr)

        tmp := parse_info.break_possible
        parse_info.break_possible = true
        defer parse_info.break_possible = tmp

        while_scope := resolve_scope(iter, parse_info, allow_single_stmt = true) or_return
        append_ast_node(node, while_scope)

        return node, ok
    }

    if !parsed_stmt && current_token(iter).type == .T_DO {
        parsed_stmt = true
        node.type = .AST_DO_WHILE

        next_token(iter)

        tmp := parse_info.break_possible
        parse_info.break_possible = true
        defer parse_info.break_possible = tmp

        while_scope := resolve_scope(iter, parse_info, allow_single_stmt = true) or_return
        append_ast_node(node, while_scope)

        if current_token(iter).type != .T_CLOSE_BRACE {
            log_error_with_token(current_token(iter)^, "Expected } at end of scope")
            return node, false
        }

        next_token(iter)

        if current_token(iter).type != .T_WHILE {
            log_error_with_token(current_token(iter)^, "Expected while after do-scope")
            return node, false
        }

        next_token(iter)

        if current_token(iter).type != .T_OPEN_PARANTHESIS {
            log_error_with_token(current_token(iter)^, "while-loop condition has to be wrapped in (...)")
            return node, false
        }

        expr := resolve_expr(node, iter, parse_info, no_expr_possible = false) or_return
        append_ast_node(node, expr)

        if current_token(iter).type != .T_SEMICOLON {
            log_error_with_token(current_token(iter)^, "Expected ; after do-while-loop")
            return node, false
        }

        return node, ok
    }

    if !parsed_stmt && current_token(iter).type == .T_FOR {
        parsed_stmt = true

        next_token(iter)

        if current_token(iter).type != .T_OPEN_PARANTHESIS {
            log_error_with_token(current_token(iter)^, "for-loop condition has to be wrapped in (...)")
            return node, false
        }

        node.type = .AST_FOR

        next_token(iter) // Skip (

        // TODO: Call resolve_statement with only declarations and assignments being allowed
        statement := resolve_statement(iter, parse_info) or_return
        append_ast_node(node, statement)

        next_token(iter) // Skip ;

        condition := resolve_expr(node, iter, parse_info) or_return
        append_ast_node(node, condition)

        next_token(iter) // Skip ;

        parse_info.no_semicolon = true
        // We check for ) because if the last statement is empty, we cant parse it correctly
        // e.g.        V This is not a valid statement otherwise
        //      for (;;) {}
        iteration := new(AstNode)
        if current_token(iter).type != .T_CLOSE_PARANTHESIS {
            iteration = resolve_statement(iter, parse_info) or_return
        }
        append_ast_node(node, iteration)
        parse_info.no_semicolon = false

        if current_token(iter).type != .T_CLOSE_PARANTHESIS {
            log_error_with_token(current_token(iter)^, "for-loop condition has to be wrapped in (...)")
            return node, false
        }
        next_token(iter) // Skip )

        tmp := parse_info.break_possible
        parse_info.break_possible = true
        defer parse_info.break_possible = tmp

        scope := resolve_scope(iter, parse_info, allow_single_stmt = true) or_return
        append_ast_node(node, scope)

        return node, ok
    }

    // Variable Assignment
    if !parsed_stmt && 
        (current_token(iter).type == .T_IDENTIFIER || 
        (current_token(iter).type == .T_STAR && look_ahead_token(iter).type == .T_IDENTIFIER)) {

        node.type = .AST_VAR_ASSIGNMENT

        if current_token(iter).type == .T_STAR {
            next_token(iter)
            node.type = .AST_VAR_DEREF_ASSIGNMENT
        }

        if token_assignment_operator(look_ahead_token(iter)^, nil, only_equal_assign = false) {
            next_token(iter)
            if !resolve_assignment(node, iter, parse_info, only_equal_assign = false) {
                return node, false
            }
            parsed_stmt = true
        } else {
            expr_node: ^AstNode
            expr_node, ok = resolve_expr(node, iter, parse_info)
            if ok && expr_node.type != .AST_NOTHING {
                append_ast_node(node, expr_node)
                parsed_stmt = true
                node.type = .AST_EXPR_STATEMENT
            }

            if expr_node.type == .AST_NOTHING {
                parsed_stmt = true
            }
        }

    }

    // Standalone expression (Yes see here)
    if !parsed_stmt {
        expr_node: ^AstNode
        expr_node, ok = resolve_expr(node, iter, parse_info)
        if ok && expr_node.type != .AST_NOTHING {
            append_ast_node(node, expr_node)
            parsed_stmt = true
            node.type = .AST_EXPR_STATEMENT
        }

        if expr_node.type == .AST_NOTHING {
            parsed_stmt = true
        }
    }

    if !parsed_stmt {
        log_error_with_token(current_token(iter)^, "Could not resolve statement")
        return node, false
    }

    // Statement ends with semicolon
    if current_token(iter).type != .T_SEMICOLON && !parse_info.no_semicolon {
        log_error_with_token(current_token(iter)^, "Statement has to end with ;")
        return node, false
    }

    return node, parsed_stmt
}

@(private="package")
resolve_function :: proc(root: ^AstNode, iter: ^TokenIter, parse_info: ^ParseInfo) -> (node: ^AstNode, ok: bool) {
    function_t: AstFunction
    node = new(AstNode)
    node.type = .AST_FUNCTION
    node.value = function_t

    // [TYPE]
    if !get_token_type_info(iter, parse_info, &function_t.ret_type) {
        log_error_with_token(current_token(iter)^, "Token is not a valid return type")
        return node, false
    }

    next_token(iter)
    
    // [TYPE] foo
    if current_token(iter).type != .T_IDENTIFIER {
        log_error_with_token(current_token(iter)^, "Token is not a valid identifier")
        return node, false
    }
    function_t.identifier = strings.clone(current_token(iter).value)

    next_token(iter)

    // [TYPE] foo(
    if current_token(iter).type != .T_OPEN_PARANTHESIS {
        log_error_with_token(current_token(iter)^, "Expected \'(\' after function identifier")
        return node, false
    }

    next_token(iter)

    // Parameter
    first_parameter := true
    for current_token(iter).type != .T_CLOSE_PARANTHESIS {
        if first_parameter && current_token(iter).value == "void" {
            if look_ahead_token(iter).type != .T_CLOSE_PARANTHESIS {
                log_error_with_token(current_token(iter)^, "Expected end of parameters after 'void'")
                return node, false
            }

            first_parameter = false
            next_token(iter)
            break;
        }

        if !first_parameter && current_token(iter).type == .T_COMMA {
            next_token(iter)
        }

        first_parameter = false

        if current_token(iter).type == .T_VARIADIC_ARGS {
            next_token(iter)

            var_args_node := new (AstNode)
            var_args_node.type = .AST_VARIADIC_ARGS
            append_ast_node(node, var_args_node)

            if current_token(iter).type != .T_CLOSE_PARANTHESIS {
                log_error_with_token(current_token(iter)^, "No args allowed after variadic arguments: ...")
                return node, false
            }

            break
        }

        parse_info.no_declare_and_assign = true
        declaration: ^AstNode
        declaration, ok = resolve_variable_declaration(iter, parse_info)
        parse_info.no_declare_and_assign = false
        if !ok {
            log_error_with_token(current_token(iter)^, "Failed to parse type declaration as parameter")
            return node, false
        }
        append_ast_node(node, declaration)

        if current_token(iter).type != .T_COMMA && current_token(iter).type != .T_CLOSE_PARANTHESIS {
            log_error_with_token(current_token(iter)^, "Expected Comma or end of parameter list")
            return node, false
        }
    }

    node.value = function_t
    next_token(iter)

    if current_token(iter).type == .T_SEMICOLON {
        node.type = .AST_FUNCTION_DECLARE
        return node, true
    }

    // [TYPE] foo(void) {
    if current_token(iter).type != .T_OPEN_BRACE {
        log_error_with_token(current_token(iter)^, "Expected \'{\' after function parameters")
        return node, false
    }

    scope: ^AstNode
    scope, ok = resolve_scope(iter, parse_info)
    append_ast_node(node, scope)
    if !ok {
        return node, false
    }

    return node, true
}

@(private="file")
resolve_scope :: proc(iter: ^TokenIter, parse_info: ^ParseInfo, allow_single_stmt := false) -> (scope_node: ^AstNode, ok: bool) {
    scope_t: AstScope
    scope_node = new(AstNode)
    scope_node.type = .AST_SCOPE
    scope_node.value = scope_t

    single_stmt := (current_token(iter).type != .T_OPEN_BRACE)
    if single_stmt && !allow_single_stmt {
        return scope_node, false
    }

    // Skip \{
    if !single_stmt do next_token(iter)

    // int foo(void) {...}
    for current_token(iter).type != .T_CLOSE_BRACE {
        statement_node: ^AstNode
        statement_node, ok = resolve_statement(iter, parse_info)
        append_ast_node(scope_node, statement_node)

        if !ok {
            return scope_node, false
        }

        if single_stmt do break

        next_token(iter)
    }

    return scope_node, true
}

@(private="file")
set_default_parse_info :: proc(parse_info: ^ParseInfo, root: ^AstNode) {
    append(&parse_info.types, DataType{size = 0, name = "void",  is_struct = false, is_float = false})
    append(&parse_info.types, DataType{size = 1, name = "char",  is_struct = false, is_float = false})
    append(&parse_info.types, DataType{size = 2, name = "short", is_struct = false, is_float = false})
    append(&parse_info.types, DataType{size = 4, name = "int",   is_struct = false, is_float = false})
    append(&parse_info.types, DataType{size = 4, name = "long",  is_struct = false, is_float = false})

    parse_info.program_node = root
}

@(private="package")
build_ast :: proc(tokens: []Token) -> (bool, ^AstNode, ^ParseInfo) {
    root := new(AstNode)
    root.type = .AST_PROGRAM

    iter: TokenIter = {
        tokens = tokens
    }

    parse_info := new(ParseInfo)
    set_default_parse_info(parse_info, root)
    // Later this struct will be filled by preprocessor, etc...

    for iter.i < len(iter.tokens) {
        node, ok := resolve_function(root, &iter, parse_info)
        append(&parse_info.functions, node.value.(AstFunction))
        append_ast_node(root, node)

        if !ok {
            log(.Error, "Could not resolve function: Aborting")
            return false, root, parse_info
        }
        next_token(&iter, fail = false)
    }

    return true, root, parse_info
}
