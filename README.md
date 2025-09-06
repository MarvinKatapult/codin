# Codin

Unfinished C-Compiler written in Odin

## Dependencies
- Odin
- Fasm

## Highest level code supported by compiler
```c
int max(int a, int b) {
    if (a > b) {
        return a;
    } 
    return b;
}

int main(void) {

    int a = 3;
    int b = 7;
    int c = 1;

    if (max(a, b) == a) {
        int tmp = a;
        a = b;
        b = tmp;
    }

    if (max(b, c) == b) {
        int tmp = a;
        a = b;
        b = tmp;
    }
    
    int result = a | b | c;

    return result;
}
```

## AST for example
```sh
[AST_PROGRAM]
    [AST_FUNCTION] Int max
        [AST_VAR_DECLARE] a  
        [AST_VAR_DECLARE] b  
        [AST_SCOPE] Parent: AST_FUNCTION 
            [AST_IF]   
                [AST_EXPR_BINARY]  OP_BINARY_GREATER Parent: AST_IF 
                    [AST_EXPR_VARIABLE] a OP Parent: AST_EXPR_BINARY 
                    [AST_EXPR_VARIABLE] b OP Parent: AST_EXPR_BINARY 
                [AST_SCOPE] Parent: AST_IF 
                    [AST_RETURN_STATEMENT]   
                        [AST_EXPR_VARIABLE] a OP Parent: AST_RETURN_STATEMENT 
            [AST_RETURN_STATEMENT]   
                [AST_EXPR_VARIABLE] b OP Parent: AST_RETURN_STATEMENT 
    [AST_FUNCTION] Int main 
        [AST_SCOPE] Parent: AST_FUNCTION 
            [AST_VAR_DECLARE] a  
                [AST_VAR_ASSIGNMENT] a  
                    [AST_EXPR_CONSTANT] 3 OP Parent: AST_VAR_ASSIGNMENT 
            [AST_VAR_DECLARE] b  
                [AST_VAR_ASSIGNMENT] b  
                    [AST_EXPR_CONSTANT] 7 OP Parent: AST_VAR_ASSIGNMENT 
            [AST_VAR_DECLARE] c  
                [AST_VAR_ASSIGNMENT] c  
                    [AST_EXPR_CONSTANT] 1 OP Parent: AST_VAR_ASSIGNMENT 
            [AST_IF]   
                [AST_EXPR_BINARY]  OP_BINARY_EQUAL Parent: AST_IF 
                    [AST_FUNC_CALL] max OP Parent: AST_EXPR_BINARY 
                        [AST_EXPR_VARIABLE] a OP Parent: AST_FUNC_CALL 
                        [AST_EXPR_VARIABLE] b OP Parent: AST_FUNC_CALL 
                    [AST_EXPR_VARIABLE] a OP Parent: AST_EXPR_BINARY 
                [AST_SCOPE] Parent: AST_IF 
                    [AST_VAR_DECLARE] tmp  
                        [AST_VAR_ASSIGNMENT] tmp  
                            [AST_EXPR_VARIABLE] a OP Parent: AST_VAR_ASSIGNMENT 
                    [AST_VAR_ASSIGNMENT] a  
                        [AST_EXPR_VARIABLE] b OP Parent: AST_VAR_ASSIGNMENT 
                    [AST_VAR_ASSIGNMENT] b  
                        [AST_EXPR_VARIABLE] tmp OP Parent: AST_VAR_ASSIGNMENT 
            [AST_IF]   
                [AST_EXPR_BINARY]  OP_BINARY_EQUAL Parent: AST_IF 
                    [AST_FUNC_CALL] max OP Parent: AST_EXPR_BINARY 
                        [AST_EXPR_VARIABLE] b OP Parent: AST_FUNC_CALL 
                        [AST_EXPR_VARIABLE] c OP Parent: AST_FUNC_CALL 
                    [AST_EXPR_VARIABLE] b OP Parent: AST_EXPR_BINARY 
                [AST_SCOPE] Parent: AST_IF 
                    [AST_VAR_DECLARE] tmp  
                        [AST_VAR_ASSIGNMENT] tmp  
                            [AST_EXPR_VARIABLE] a OP Parent: AST_VAR_ASSIGNMENT 
                    [AST_VAR_ASSIGNMENT] a  
                        [AST_EXPR_VARIABLE] b OP Parent: AST_VAR_ASSIGNMENT 
                    [AST_VAR_ASSIGNMENT] b  
                        [AST_EXPR_VARIABLE] tmp OP Parent: AST_VAR_ASSIGNMENT 
            [AST_VAR_DECLARE] result  
                [AST_VAR_ASSIGNMENT] result  
                    [AST_EXPR_BINARY]  OP_BINARY_PLUS Parent: AST_VAR_ASSIGNMENT 
                        [AST_EXPR_BINARY]  OP_BINARY_DIV Parent: AST_EXPR_BINARY 
                            [AST_EXPR_BINARY]  OP_BIT_SHL Parent: AST_EXPR_BINARY 
                                [AST_EXPR_BINARY]  OP_BIT_SHL Parent: AST_EXPR_BINARY 
                                    [AST_EXPR_VARIABLE] a OP Parent: AST_EXPR_BINARY 
                                    [AST_EXPR_VARIABLE] b OP Parent: AST_EXPR_BINARY 
                                [AST_EXPR_VARIABLE] c OP Parent: AST_EXPR_BINARY 
                            [AST_EXPR_CONSTANT] 2 OP Parent: AST_EXPR_BINARY 
                        [AST_EXPR_CONSTANT] 13 OP Parent: AST_EXPR_BINARY 
            [AST_RETURN_STATEMENT]   
                [AST_EXPR_VARIABLE] result OP Parent: AST_RETURN_STATEMENT
```
