# Codin

Unfinished C-Compiler written in Odin

## Dependencies
- Odin
- Fasm

## Highest level code supported by compiler
```c
int main(void) {
    // We only have signed integers (64-bit)
    int a = 0;
    int b = 0;
    while (a < 100) {
        if (!(a % 2)) {
            b = b + a;
        }
        a = a + 1;

        if (b > 1000) {
            break;
        }
    }
    
    return b;
}
```
