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
