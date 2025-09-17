# Codin

Unfinished C-Compiler written in Odin

## Dependencies
- Odin
- Nasm
- ld
- C-Runtime
- Linux Dynamic Linker

## Highest level code supported by compiler
```c
int putchar(int n);

int countdigits(int n) {
	int ret = 0;
	if (n == 0) return 1;

	while (n > 0) {
		ret += 1;
		n /= 10;
	}
	return ret;
}

int exp(int base, int exponent) {
	int ret = base;
	if (exponent == 0) return 1;
	for (int i = 1; i < exponent; i += 1) {
		ret *= base;
	}
	return ret;
}

int get_left_most_digit(int n) {
	int count_digits = countdigits(n);
	if (count_digits == 1) return n;
	n /= exp(10, count_digits-1);
	return n;
}

void printnum(int a) {

	if (a == 0) putchar('0');
	else {
		while (a > 0) {
			int left_most = get_left_most_digit(a);
			putchar(left_most + '0');

			int count_digits = countdigits(a);
			a -= left_most * exp(10, count_digits - 1);
		}
	}
	
	putchar('\n');
}

int main(void) {

	printnum(55);

	return 0;
}
```

## AST for example
```text
[AST_PROGRAM]
    [AST_FUNCTION_DECLARE] putchar DataType{name = "int", size = 4, is_float = false, unsigned = false, is_struct = false}
        [AST_VAR_DECLARE] n
    [AST_FUNCTION] countdigits DataType{name = "int", size = 4, is_float = false, unsigned = false, is_struct = false}
        [AST_VAR_DECLARE] n
        [AST_SCOPE] Parent: AST_FUNCTION
            [AST_VAR_DECLARE] ret
                [AST_VAR_ASSIGNMENT] ret
                    [AST_EXPR_CONSTANT] 0 OP Parent: AST_VAR_ASSIGNMENT
            [AST_IF]
                [AST_EXPR_BINARY]  OP_BINARY_EQUAL Parent: AST_IF
                    [AST_EXPR_VARIABLE] n OP Parent: AST_EXPR_BINARY
                    [AST_EXPR_CONSTANT] 0 OP Parent: AST_EXPR_BINARY
                [AST_SCOPE] Parent: AST_IF
                    [AST_RETURN_STATEMENT]
                        [AST_EXPR_CONSTANT] 1 OP Parent: AST_RETURN_STATEMENT
            [AST_WHILE]
                [AST_EXPR_BINARY]  OP_BINARY_GREATER Parent: AST_WHILE
                    [AST_EXPR_VARIABLE] n OP Parent: AST_EXPR_BINARY
                    [AST_EXPR_CONSTANT] 0 OP Parent: AST_EXPR_BINARY
                [AST_SCOPE] Parent: AST_WHILE
                    [AST_VAR_ASSIGNMENT] ret
                        [AST_EXPR_BINARY]  OP_BINARY_PLUS Parent: AST_VAR_ASSIGNMENT
                            [AST_EXPR_VARIABLE] ret OP_BINARY_PLUS Parent: AST_EXPR_BINARY
                            [AST_EXPR_CONSTANT] 1 OP Parent: AST_EXPR_BINARY
                    [AST_VAR_ASSIGNMENT] n
                        [AST_EXPR_BINARY]  OP_BINARY_DIV Parent: AST_VAR_ASSIGNMENT
                            [AST_EXPR_VARIABLE] n OP_BINARY_DIV Parent: AST_EXPR_BINARY
                            [AST_EXPR_CONSTANT] 10 OP Parent: AST_EXPR_BINARY
            [AST_RETURN_STATEMENT]
                [AST_EXPR_VARIABLE] ret OP Parent: AST_RETURN_STATEMENT
    [AST_FUNCTION] exp DataType{name = "int", size = 4, is_float = false, unsigned = false, is_struct = false}
        [AST_VAR_DECLARE] base
        [AST_VAR_DECLARE] exponent
        [AST_SCOPE] Parent: AST_FUNCTION
            [AST_VAR_DECLARE] ret
                [AST_VAR_ASSIGNMENT] ret
                    [AST_EXPR_VARIABLE] base OP Parent: AST_VAR_ASSIGNMENT
            [AST_IF]
                [AST_EXPR_BINARY]  OP_BINARY_EQUAL Parent: AST_IF
                    [AST_EXPR_VARIABLE] exponent OP Parent: AST_EXPR_BINARY
                    [AST_EXPR_CONSTANT] 0 OP Parent: AST_EXPR_BINARY
                [AST_SCOPE] Parent: AST_IF
                    [AST_RETURN_STATEMENT]
                        [AST_EXPR_CONSTANT] 1 OP Parent: AST_RETURN_STATEMENT
            [AST_FOR]
                [AST_VAR_DECLARE] i
                    [AST_VAR_ASSIGNMENT] i
                        [AST_EXPR_CONSTANT] 1 OP Parent: AST_VAR_ASSIGNMENT
                [AST_EXPR_BINARY]  OP_BINARY_LESS Parent: AST_FOR
                    [AST_EXPR_VARIABLE] i OP Parent: AST_EXPR_BINARY
                    [AST_EXPR_VARIABLE] exponent OP Parent: AST_EXPR_BINARY
                [AST_VAR_ASSIGNMENT] i
                    [AST_EXPR_BINARY]  OP_BINARY_PLUS Parent: AST_VAR_ASSIGNMENT
                        [AST_EXPR_VARIABLE] i OP_BINARY_PLUS Parent: AST_EXPR_BINARY
                        [AST_EXPR_CONSTANT] 1 OP Parent: AST_EXPR_BINARY
                [AST_SCOPE] Parent: AST_FOR
                    [AST_VAR_ASSIGNMENT] ret
                        [AST_EXPR_BINARY]  OP_BINARY_MULT Parent: AST_VAR_ASSIGNMENT
                            [AST_EXPR_VARIABLE] ret OP_BINARY_MULT Parent: AST_EXPR_BINARY
                            [AST_EXPR_VARIABLE] base OP Parent: AST_EXPR_BINARY
            [AST_RETURN_STATEMENT]
                [AST_EXPR_VARIABLE] ret OP Parent: AST_RETURN_STATEMENT
    [AST_FUNCTION] get_left_most_digit DataType{name = "int", size = 4, is_float = false, unsigned = false, is_struct = false}
        [AST_VAR_DECLARE] n
        [AST_SCOPE] Parent: AST_FUNCTION
            [AST_VAR_DECLARE] count_digits
                [AST_VAR_ASSIGNMENT] count_digits
                    [AST_FUNC_CALL] countdigits OP Parent: AST_VAR_ASSIGNMENT
                        [AST_EXPR_VARIABLE] n OP Parent: AST_FUNC_CALL
            [AST_IF]
                [AST_EXPR_BINARY]  OP_BINARY_EQUAL Parent: AST_IF
                    [AST_EXPR_VARIABLE] count_digits OP Parent: AST_EXPR_BINARY
                    [AST_EXPR_CONSTANT] 1 OP Parent: AST_EXPR_BINARY
                [AST_SCOPE] Parent: AST_IF
                    [AST_RETURN_STATEMENT]
                        [AST_EXPR_VARIABLE] n OP Parent: AST_RETURN_STATEMENT
            [AST_VAR_ASSIGNMENT] n
                [AST_EXPR_BINARY]  OP_BINARY_DIV Parent: AST_VAR_ASSIGNMENT
                    [AST_EXPR_VARIABLE] n OP_BINARY_DIV Parent: AST_EXPR_BINARY
                    [AST_FUNC_CALL] exp OP Parent: AST_EXPR_BINARY
                        [AST_EXPR_CONSTANT] 10 OP Parent: AST_FUNC_CALL
                        [AST_EXPR_BINARY]  OP_BINARY_MINUS Parent: AST_FUNC_CALL
                            [AST_EXPR_VARIABLE] count_digits OP Parent: AST_EXPR_BINARY
                            [AST_EXPR_CONSTANT] 1 OP Parent: AST_EXPR_BINARY
            [AST_RETURN_STATEMENT]
                [AST_EXPR_VARIABLE] n OP Parent: AST_RETURN_STATEMENT
    [AST_FUNCTION] printnum DataType{name = "void", size = 0, is_float = false, unsigned = false, is_struct = false}
        [AST_VAR_DECLARE] a
        [AST_SCOPE] Parent: AST_FUNCTION
            [AST_IF]
                [AST_EXPR_BINARY]  OP_BINARY_EQUAL Parent: AST_IF
                    [AST_EXPR_VARIABLE] a OP Parent: AST_EXPR_BINARY
                    [AST_EXPR_CONSTANT] 0 OP Parent: AST_EXPR_BINARY
                [AST_SCOPE] Parent: AST_IF
                    [AST_EXPR_STATEMENT]
                        [AST_FUNC_CALL] putchar OP Parent: AST_EXPR_STATEMENT
                            [AST_EXPR_CONSTANT] 48 OP Parent: AST_FUNC_CALL
                [AST_ELSE]
                    [AST_SCOPE] Parent: AST_ELSE
                        [AST_WHILE]
                            [AST_EXPR_BINARY]  OP_BINARY_GREATER Parent: AST_WHILE
                                [AST_EXPR_VARIABLE] a OP Parent: AST_EXPR_BINARY
                                [AST_EXPR_CONSTANT] 0 OP Parent: AST_EXPR_BINARY
                            [AST_SCOPE] Parent: AST_WHILE
                                [AST_VAR_DECLARE] left_most
                                    [AST_VAR_ASSIGNMENT] left_most
                                        [AST_FUNC_CALL] get_left_most_digit OP Parent: AST_VAR_ASSIGNMENT
                                            [AST_EXPR_VARIABLE] a OP Parent: AST_FUNC_CALL
                                [AST_EXPR_STATEMENT]
                                    [AST_FUNC_CALL] putchar OP Parent: AST_EXPR_STATEMENT
                                        [AST_EXPR_BINARY]  OP_BINARY_PLUS Parent: AST_FUNC_CALL
                                            [AST_EXPR_VARIABLE] left_most OP Parent: AST_EXPR_BINARY
                                            [AST_EXPR_CONSTANT] 48 OP Parent: AST_EXPR_BINARY
                                [AST_VAR_DECLARE] count_digits
                                    [AST_VAR_ASSIGNMENT] count_digits
                                        [AST_FUNC_CALL] countdigits OP Parent: AST_VAR_ASSIGNMENT
                                            [AST_EXPR_VARIABLE] a OP Parent: AST_FUNC_CALL
                                [AST_VAR_ASSIGNMENT] a
                                    [AST_EXPR_BINARY]  OP_BINARY_MINUS Parent: AST_VAR_ASSIGNMENT
                                        [AST_EXPR_VARIABLE] a OP_BINARY_MINUS Parent: AST_EXPR_BINARY
                                        [AST_EXPR_BINARY]  OP_BINARY_MULT Parent: AST_EXPR_BINARY
                                            [AST_EXPR_VARIABLE] left_most OP Parent: AST_EXPR_BINARY
                                            [AST_FUNC_CALL] exp OP Parent: AST_EXPR_BINARY
                                                [AST_EXPR_CONSTANT] 10 OP Parent: AST_FUNC_CALL
                                                [AST_EXPR_BINARY]  OP_BINARY_MINUS Parent: AST_FUNC_CALL
                                                    [AST_EXPR_VARIABLE] count_digits OP Parent: AST_EXPR_BINARY
                                                    [AST_EXPR_CONSTANT] 1 OP Parent: AST_EXPR_BINARY
            [AST_EXPR_STATEMENT]
                [AST_FUNC_CALL] putchar OP Parent: AST_EXPR_STATEMENT
                    [AST_EXPR_CONSTANT] 10 OP Parent: AST_FUNC_CALL
    [AST_FUNCTION] main DataType{name = "int", size = 4, is_float = false, unsigned = false, is_struct = false}
        [AST_SCOPE] Parent: AST_FUNCTION
            [AST_EXPR_STATEMENT]
                [AST_FUNC_CALL] printnum OP Parent: AST_EXPR_STATEMENT
                    [AST_EXPR_CONSTANT] 55 OP Parent: AST_FUNC_CALL
            [AST_RETURN_STATEMENT]
                [AST_EXPR_CONSTANT] 0 OP Parent: AST_RETURN_STATEMENT
```
