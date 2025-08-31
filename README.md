# Codin

This is my hobby C Compiler project written in Odin!

I always wanted to build a C Compiler so this is it.

It's not finished and even if it was, it wouldn't be good, but that's not the goal of this project.

## Dependencies
- Odin
- Fasm

## Highest level code we can compile
```c
int main(void) {
    int a = (3 - 2) * 2;
    int b;
    {
        int c = a * 3;
        if (c + 1) {
            c = 69 + 420;
        } else {
            c = 1337 * 2;
        }
        b = 4 * c | 2 && 1;
    }
    ;;;;
    return b ^ 1;
}
```
