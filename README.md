# SimpleForth

This is a simple forth interpreter written in Haskell.

It only supports integer data and the following commands in Forth:

- `integer (eg. 12)`  (push number to stack)
- `+` (fetch two numbers from stack, add them, push result back to stack)
- `-` (fetch two numbers from stack, substract them, push result back to stack)
- `*` (fetch two numbers from stack, multiply them, push result back to stack)
- `/` (fetch two numbers from stack, divide them, push result back to stack)
- `mod` (fetch two numbers from stack, mod on them, push result back to stack)
- `negate` (fetch one  number from stack, negate it, push result back to stack)
- `abs` (fetch one  number from stack, calculae it absolute value, push result back to stack)
- `dup` (duplicate the top number of stack)
- `swap` (swap the top two numbers of stack)
- `drop` (drop the top number of stack)
- `.` (pop the top number of stack, show it to console)
- `.s` (show the whole stack to console)
- `quit` (quit the interpreter)
- other input (clear the stack)

## How to compile
Build the program using cabal:
```
cabal build
```

## How to run
Run the program using cabal:
```
cabal run
```
or
```
cabal exec SimpleForth
```


