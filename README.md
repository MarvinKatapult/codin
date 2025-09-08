# Codin

Unfinished C-Compiler written in Odin

## Dependencies
- Odin
- Fasm

## Highest level code supported by compiler
```c
int fib(int n) 
{
	if (n <= 2) {
		return 1;
	}

	return fib(n-1) + fib(n-2);
}

int main(void) 
{
	int a = fib(13);
    return a; // Exit-Code:233
}
```

## AST for example
```text
[AST_PROGRAM][0m
    [AST_FUNCTION] Int fib 
        [AST_VAR_DECLARE] n  
        [AST_SCOPE] Parent: AST_FUNCTION 
            [AST_IF]   
                [AST_EXPR_BINARY]  OP_BINARY_LESS_EQUAL Parent: AST_IF 
                    [AST_EXPR_VARIABLE] n OP Parent: AST_EXPR_BINARY 
                    [AST_EXPR_CONSTANT] 2 OP Parent: AST_EXPR_BINARY 
                [AST_SCOPE] Parent: AST_IF 
                    [AST_RETURN_STATEMENT]   
                        [AST_EXPR_CONSTANT] 1 OP Parent: AST_RETURN_STATEMENT 
            [AST_RETURN_STATEMENT]   
                [AST_EXPR_BINARY]  OP_BINARY_PLUS Parent: AST_RETURN_STATEMENT 
                    [AST_FUNC_CALL] fib OP Parent: AST_EXPR_BINARY 
                        [AST_EXPR_BINARY]  OP_BINARY_MINUS Parent: AST_FUNC_CALL 
                            [AST_EXPR_VARIABLE] n OP Parent: AST_EXPR_BINARY 
                            [AST_EXPR_CONSTANT] 1 OP Parent: AST_EXPR_BINARY 
                    [AST_FUNC_CALL] fib OP Parent: AST_EXPR_BINARY 
                        [AST_EXPR_BINARY]  OP_BINARY_MINUS Parent: AST_FUNC_CALL 
                            [AST_EXPR_VARIABLE] n OP Parent: AST_EXPR_BINARY 
                            [AST_EXPR_CONSTANT] 2 OP Parent: AST_EXPR_BINARY 
    [AST_FUNCTION] Int main 
        [AST_SCOPE] Parent: AST_FUNCTION 
            [AST_VAR_DECLARE] a  
                [AST_VAR_ASSIGNMENT] a  
                    [AST_FUNC_CALL] fib OP Parent: AST_VAR_ASSIGNMENT 
                        [AST_EXPR_CONSTANT] 13 OP Parent: AST_FUNC_CALL 
            [AST_RETURN_STATEMENT]   
                [AST_EXPR_VARIABLE] a OP Parent: AST_RETURN_STATEMENT 
```
