# Codin

Unfinished C-Compiler written in Odin

## Dependencies
- Odin
- Fasm

## Highest level code supported by compiler
```c
int fac(int n) 
{
	if (n <= 1) {
		return n;
	}
	return n * fac(n-1);
}

int main(void) 
{
	int a = fac(4) + 1;
    return a;
}
}
```

## AST for example
```text
[AST_PROGRAM]
    [AST_FUNCTION] Int fac 
        [AST_VAR_DECLARE] n  
        [AST_SCOPE] Parent: AST_FUNCTION 
            [AST_IF]   
                [AST_EXPR_BINARY]  OP_BINARY_LESS_EQUAL Parent: AST_IF 
                    [AST_EXPR_VARIABLE] n OP Parent: AST_EXPR_BINARY 
                    [AST_EXPR_CONSTANT] 1 OP Parent: AST_EXPR_BINARY 
                [AST_SCOPE] Parent: AST_IF 
                    [AST_RETURN_STATEMENT]   
                        [AST_EXPR_VARIABLE] n OP Parent: AST_RETURN_STATEMENT 
            [AST_RETURN_STATEMENT]   
                [AST_EXPR_BINARY]  OP_BINARY_MULT Parent: AST_RETURN_STATEMENT 
                    [AST_EXPR_VARIABLE] n OP Parent: AST_EXPR_BINARY 
                    [AST_FUNC_CALL] fac OP Parent: AST_EXPR_BINARY 
                        [AST_EXPR_BINARY]  OP_BINARY_MINUS Parent: AST_FUNC_CALL 
                            [AST_EXPR_VARIABLE] n OP Parent: AST_EXPR_BINARY 
                            [AST_EXPR_CONSTANT] 1 OP Parent: AST_EXPR_BINARY 
    [AST_FUNCTION] Int main 
        [AST_SCOPE] Parent: AST_FUNCTION 
            [AST_VAR_DECLARE] a  
                [AST_VAR_ASSIGNMENT] a  
                    [AST_EXPR_BINARY]  OP_BINARY_PLUS Parent: AST_VAR_ASSIGNMENT 
                        [AST_FUNC_CALL] fac OP Parent: AST_EXPR_BINARY 
                            [AST_EXPR_CONSTANT] 4 OP Parent: AST_FUNC_CALL 
                        [AST_EXPR_CONSTANT] 1 OP Parent: AST_EXPR_BINARY 
            [AST_RETURN_STATEMENT]   
                [AST_EXPR_VARIABLE] a OP Parent: AST_RETURN_STATEMENT 
```
