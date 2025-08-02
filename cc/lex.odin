package cc

import "core:os"
import "core:strings"
import "core:strconv"
import "core:unicode/utf8"
import "core:fmt"

@(private="file")
Type :: enum {
    T_OPEN_BRACE,
    T_CLOSE_BRACE,
    T_OPEN_PARANTHESIS,
    T_CLOSE_PARANTHESIS,
    T_SEMICOLON,
    T_RETURN_KEYWORD,
    T_INT_KEYWORD,
    T_VOID_KEYWORD,
    T_IDENTIFIER,
    T_INTEGER_LITERAL,
}

@(private="package")
Token :: struct {
    value: string,
    type: Type,
    x: u32,
    y: u32,
}

@(private="package")
WHITESPACE :: "\t\n\v\f\r "

cleanup_tokens :: proc(tokens: ^[dynamic]Token) {
    for token in tokens {
        delete(token.value);
    }
    delete(tokens^);
}

@(private="file")
is_special_symbol :: proc(symbol: rune) -> (bool, Type) {
    switch (symbol) {
        case '{':
            return true, Type.T_OPEN_BRACE;
        case '}':
            return true, Type.T_CLOSE_BRACE;
        case '(':
            return true, Type.T_OPEN_PARANTHESIS;
        case ')':
            return true, Type.T_CLOSE_PARANTHESIS;
        case ';':
            return true, Type.T_SEMICOLON;
    }

    return false, nil;
}

@(private="file")
is_keyword :: proc(s: string) -> (bool, Type) {
    switch (s) {
        case "return":
            return true, Type.T_RETURN_KEYWORD;
        case "int":
            return true, Type.T_INT_KEYWORD;
        case "void":
            return true, Type.T_VOID_KEYWORD;
    }
    return false, Type.T_IDENTIFIER;
}

@(private="file")
check_for_keyword :: proc(s: string, tokens: ^[dynamic]Token, x: u32, y: u32) -> bool {
    if (len(strings.trim(s, WHITESPACE)) > 0) {
        token: Token;
        is_keyword, type := is_keyword(s);

        token.value = strings.clone_from(s);
        token.type = type;
        token.x = x - u32(len(s));
        token.y = y;

        append(tokens, token);
        return true;
    }
    return false;
}

@(private="file")
check_for_int_literal :: proc(s: string, tokens: ^[dynamic]Token, x: u32, y: u32) -> bool {
    _, is_int := strconv.parse_int(s);
    if (is_int) {
        token: Token;
        token.type = Type.T_INTEGER_LITERAL;
        token.value = strings.clone_from(s);
        token.x = x - u32(len(s));
        token.y = y;
        append(tokens, token);
        return true;
    }
    return false;
}

lex :: proc(filename: string) -> [dynamic]Token {
    ret: [dynamic]Token;

    filedata, ok := os.read_entire_file(filename);
    if !ok {
        fmt.println("Cant read file!");
        return nil;
    }
    defer delete(filedata);

    file_str := string(filedata);

    str_b := strings.builder_make();
    defer strings.builder_destroy(&str_b);
    token: Token;
    x: u32 = 1;
    y: u32 = 1;
    in_line_comment: bool = false;
    for c, i in file_str {
        defer x += 1;


        if (in_line_comment) do continue;

        buf := strings.to_string(str_b);

        if (c == '/' && len(file_str) > i + 1 && file_str[i + 1] == '/') {
            in_line_comment = true;
            continue;
        }

        is_space := strings.is_space(c);
        special_symbol, type := is_special_symbol(c);

        if (is_space || special_symbol) {
            // If buffer is still filled, identify string
            if check_for_int_literal(buf, &ret, x, y) do strings.builder_reset(&str_b);
            else if check_for_keyword(buf, &ret, x, y) do strings.builder_reset(&str_b);
        }

        if (is_space) {
            if (c == '\n') {
                y += 1;
                x = 0;
                in_line_comment = false;
            }
            continue;
        }

        // Is rune special symbol that is a token itself?
        if (special_symbol) {
            // Append special symbol as token
            token.type = type;
            token.value = utf8.runes_to_string([]rune{c});
            token.x = x;
            token.y = y;
            append(&ret, token);
            continue;
        }

        strings.write_rune(&str_b, c);
    }

    return ret;
}

