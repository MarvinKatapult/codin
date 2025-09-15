package cc

import "core:os"
import "core:strings"
import "core:strconv"
import "core:unicode/utf8"
import "core:fmt"

@(private="package")
TokenType :: enum {
	T_OPEN_BRACE,
	T_CLOSE_BRACE,
	T_OPEN_PARANTHESIS,
	T_CLOSE_PARANTHESIS,
	T_SEMICOLON,
	T_RETURN_KEYWORD,
	T_IF,
	T_ELSE,
	T_WHILE,
	T_FOR,
	T_IDENTIFIER,
	T_INT_LITERAL,
	T_PLUS,
	T_MINUS,
	T_STAR,
	T_FSLASH,
	T_PERCENT,
	T_TILDE,
	T_EXCLAMATION,
	T_LESS,
	T_GREATER,
	T_LESS_EQUAL,
	T_GREATER_EQUAL,
	T_ASSIGNMENT,
	T_ASSIGN_PLUS,
	T_ASSIGN_MINUS,
	T_ASSIGN_MULT,
	T_ASSIGN_DIV,
	T_ASSIGN_PERCENT,
	T_ASSIGN_SHIFTL,
	T_ASSIGN_SHIFTR,
	T_ASSIGN_AND,
	T_ASSIGN_XOR,
	T_ASSIGN_OR,
	T_EQUAL_EQUAL,
	T_NOT_EQUAL,
	T_BIT_AND,
	T_BIT_OR,
	T_BIT_XOR,
	T_LOGICAL_AND,
	T_LOGICAL_OR,
	T_SHIFT_LEFT,
	T_SHIFT_RIGHT,
	T_BREAK,
	T_COMMA,
	T_UNSIGNED,
	T_SIGNED,
}

@(private="package")
Token :: struct {
	value: string,
	type: TokenType,
	x: u32,
	y: u32,
}

@(private="package")
WHITESPACE :: "\t\n\v\f\r "

@(private="file")
is_special_symbol :: proc(symbol: rune) -> (bool, TokenType) {
	switch (symbol) {
		case '{':
			return true, TokenType.T_OPEN_BRACE
		case '}':
			return true, TokenType.T_CLOSE_BRACE
		case '(':
			return true, TokenType.T_OPEN_PARANTHESIS
		case ')':
			return true, TokenType.T_CLOSE_PARANTHESIS
		case ';':
			return true, TokenType.T_SEMICOLON
		case '!':
			return true, TokenType.T_EXCLAMATION
		case '~':
			return true, TokenType.T_TILDE
		case '-':
			return true, TokenType.T_MINUS
		case '+':
			return true, TokenType.T_PLUS
		case '*':
			return true, TokenType.T_STAR
		case '/':
			return true, TokenType.T_FSLASH
		case '%':
			return true, TokenType.T_PERCENT
		case '<':
			return true, TokenType.T_LESS
		case '>':
			return true, TokenType.T_GREATER
		case '=':
			return true, TokenType.T_ASSIGNMENT
		case '&':
			return true, TokenType.T_BIT_AND
		case '|':
			return true, TokenType.T_BIT_OR
		case '^':
			return true, TokenType.T_BIT_XOR
		case ',':
			return true, TokenType.T_COMMA
	}

	return false, nil
}

@(private="file")
is_keyword :: proc(s: string) -> (bool, TokenType) {
	switch (s) {
		case "return":
			return true, TokenType.T_RETURN_KEYWORD
		case "if":
			return true, TokenType.T_IF
		case "else":
			return true, TokenType.T_ELSE
		case "while":
			return true, TokenType.T_WHILE
		case "for":
			return true, TokenType.T_FOR
		case "break":
			return true, TokenType.T_BREAK
		case "unsigned":
			return true, TokenType.T_UNSIGNED
		case "signed":
			return true, TokenType.T_SIGNED
	}
	return false, TokenType.T_IDENTIFIER
}

@(private="file")
check_for_keyword :: proc(s: string, tokens: ^[dynamic]Token, x: u32, y: u32) -> bool {
	if (len(strings.trim(s, WHITESPACE)) > 0) {
		token: Token
		is_keyword, type := is_keyword(s)

		token.value = strings.clone_from(s)
		token.type = type
		token.x = x - u32(len(s))
		token.y = y

		append(tokens, token)
		return true
	}
	return false
}

@(private="file")
check_for_int_literal :: proc(s: string, tokens: ^[dynamic]Token, x: u32, y: u32) -> bool {
	_, is_int := strconv.parse_int(s)
	if (is_int) {
		token: Token
		token.type = TokenType.T_INT_LITERAL
		token.value = strings.clone_from(s)
		token.x = x - u32(len(s))
		token.y = y
		append(tokens, token)
		return true
	}
	return false
}

@(private="file")
look_ahead_one :: proc(buf: string, index: int, c1: u8, c2: u8) -> bool {
	c := buf[index]
	return (len(buf) >= index && buf[index] == c1 && buf[index + 1] == c2)
}

@(private="file")
look_back_one :: proc(buf: string, index: int, c1: u8, c2: u8) -> bool {
	c := buf[index]
	return (index > 0 && buf[index - 1] == c1 && buf[index] == c2)
}

@(private="package")
lex :: proc(filename: string) -> [dynamic]Token {
	ret: [dynamic]Token

	filedata, ok := os.read_entire_file(filename)
	if !ok {
		log(.Error, "Cant read file!")
		return nil
	}
	defer delete(filedata)

	file_str := string(filedata)

	str_b := strings.builder_make()
	defer strings.builder_destroy(&str_b)
	token: Token
	x: u32 = 1
	y: u32 = 1
	in_line_comment: bool = false
	comment: bool = false
	for c, i in file_str {
		defer x += 1

		last_token := i == len(file_str) - 1
		if last_token {
			strings.write_rune(&str_b, c)
		}

		if look_ahead_one(file_str, i, '/', '/') {
			in_line_comment = true
			continue
		}

		if look_ahead_one(file_str, i, '/', '*') {
			comment = true
			continue
		}
		
		if look_back_one(file_str, i, '*', '/') {
			comment = false
			continue
		}


		buf := strings.trim(strings.to_string(str_b), WHITESPACE)

		if (c == '\n') {
			y += 1
			x = 0
			in_line_comment = false
		}

		if in_line_comment || comment do continue

		is_space := strings.is_space(c)
		special_symbol, type := is_special_symbol(c)

		if c == '=' && i != 0 {
			last_token := &ret[len(ret)-1]
			#partial switch last_token.type {
				case .T_LESS:
					last_token.type  = .T_LESS_EQUAL
					delete(last_token.value)
					last_token.value = "<="
					continue;
				case .T_GREATER:
					last_token.type = .T_GREATER_EQUAL
					delete(last_token.value)
					last_token.value = ">="
					continue;
				case .T_ASSIGNMENT:
					last_token.type = .T_EQUAL_EQUAL
					delete(last_token.value)
					last_token.value = "=="
					continue;
				case .T_EXCLAMATION:
					last_token.type = .T_NOT_EQUAL
					delete(last_token.value)
					last_token.value = "!="
					continue;
				case .T_PLUS:
					last_token.type = .T_ASSIGN_PLUS
					delete(last_token.value)
					last_token.value = "+="
					continue;
				case .T_MINUS:
					last_token.type = .T_ASSIGN_MINUS
					delete(last_token.value)
					last_token.value = "-="
					continue;
				case .T_STAR:
					last_token.type = .T_ASSIGN_MULT
					delete(last_token.value)
					last_token.value = "*="
					continue;
				case .T_FSLASH:
					last_token.type = .T_ASSIGN_DIV
					delete(last_token.value)
					last_token.value = "/="
					continue;
				case .T_PERCENT:
					last_token.type = .T_ASSIGN_PERCENT
					delete(last_token.value)
					last_token.value = "%="
					continue;
				case .T_SHIFT_LEFT:
					last_token.type = .T_ASSIGN_SHIFTL
					delete(last_token.value)
					last_token.value = "<<="
					continue;
				case .T_SHIFT_RIGHT:
					last_token.type = .T_ASSIGN_SHIFTR
					delete(last_token.value)
					last_token.value = ">>="
					continue;
				case .T_BIT_AND:
					last_token.type = .T_ASSIGN_AND
					delete(last_token.value)
					last_token.value = "&="
					continue;
				case .T_BIT_XOR:
					last_token.type = .T_ASSIGN_XOR
					delete(last_token.value)
					last_token.value = "^="
					continue;
				case .T_BIT_OR:
					last_token.type = .T_ASSIGN_OR
					delete(last_token.value)
					last_token.value = "|="
					continue;
			}
		}

		if c == '<' && i != 0 {
			last_token := &ret[len(ret)-1]
			if last_token.type == .T_LESS {
				last_token.type = .T_SHIFT_LEFT
				delete(last_token.value)
				last_token.value = "<<"
				continue
			}
		}

		if c == '>' && i != 0 {
			last_token := &ret[len(ret)-1]
			if last_token.type == .T_GREATER {
				last_token.type = .T_SHIFT_RIGHT
				delete(last_token.value)
				last_token.value = ">>"
				continue
			}
		}

		if c == '&' && i != 0 {
			last_token := &ret[len(ret)-1]
			if last_token.type == .T_BIT_AND {
				last_token.type = .T_LOGICAL_AND
				delete(last_token.value)
				last_token.value = "&&"
				continue;
			}
		}

		if c == '|' && i != 0 {
			last_token := &ret[len(ret)-1]
			if last_token.type == .T_BIT_OR {
				last_token.type = .T_LOGICAL_OR
				delete(last_token.value)
				last_token.value = "||"
				continue;
			}
		}

		if is_space && len(buf) == 0 {
			strings.builder_reset(&str_b)
			continue
		}

		parse_token := is_space || special_symbol || last_token

		if parse_token {
			// If buffer is still filled, identify string
			if check_for_int_literal(buf, &ret, x, y) do strings.builder_reset(&str_b)
			else if check_for_keyword(buf, &ret, x, y) do strings.builder_reset(&str_b)
		}

		// Is rune special symbol that is a token itself?
		if special_symbol {
			// Append special symbol as token
			token.type = type
			token.value = utf8.runes_to_string([]rune{c})
			token.x = x
			token.y = y
			append(&ret, token)
			continue
		}

		strings.write_rune(&str_b, c)
	}

	return ret
}
