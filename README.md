# mips-compiler

stack exec mips-compiler-exe test.input

Input:
``` Rust
x = 3;
y = 4;
z = 5;
if x == 2 {
    a = 0 + x + 3;
} else if x == 3 {
    a = x + (y - z) + 2 + 4 - y + 8 + z;
} else {
    if x == 1 {
        a = y + 2 + 3;
    } else {
        a = z + 4 + y;
    }
}
save a 0x8080;
```

Output:
```
L1: NOP
    a = 17
    SAVE "a" 32896
    RETURN
```
