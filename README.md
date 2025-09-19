# Codin

Unfinished C-Compiler written in Odin

## Dependencies
- Odin 					(https://github.com/odin-lang/Odin)
- Nasm					(https://github.com/netwide-assembler/nasm)
- gcc-multilib			(apt: gcc-multilib)
- ld					(coreutil)
- Linux Dynamic Linker  (coreutil)

## Highest level code supported by compiler
```c
void printf(int str, ...);

int main(void) {

	printf("This is a cool number: %d%c", 420, '\n');

	return 0;
}
```

## AST for example
```text
[AST_PROGRAM]
    [AST_STRING_LITERAL] GBL_0 This is a cool number: %d%c
    [AST_FUNCTION_DECLARE] printf DataType{name = "void", size = 0, is_float = false, unsigned = false, is_struct = false, variadic = false}
        [AST_VAR_DECLARE] str
        [AST_VARIADIC_ARGS]
    [AST_FUNCTION] main DataType{name = "int", size = 4, is_float = false, unsigned = false, is_struct = false, variadic = false}
        [AST_SCOPE] Parent: AST_FUNCTION
            [AST_EXPR_STATEMENT]
                [AST_FUNC_CALL] printf OP Parent: AST_EXPR_STATEMENT
                    [AST_STRING_LITERAL] GBL_0 OP Parent: AST_FUNC_CALL
                    [AST_EXPR_CONSTANT] 420 OP Parent: AST_FUNC_CALL
                    [AST_EXPR_CONSTANT] 10 OP Parent: AST_FUNC_CALL
            [AST_RETURN_STATEMENT]
                [AST_EXPR_CONSTANT] 0 OP Parent: AST_RETURN_STATEMENT
```
