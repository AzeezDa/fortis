# Fortis - A 4-bit-Instruction-8-bit-Register-12-bit-addressing Computer

# Instruction Set Architecture

## Registers
| Register | Description |
| -------- | ----------- |
| R1 | 8-bit Register 1 |
| R2 | 8-bit Register 2 |
| SP | 12-bit Stack Pointer, starts at 0xFFF |
| PC | 12-bit Program Counter, starts at 0x000 |

Notes: The SP can span the entire memory space but the PC will use 11 bits for the memory access and 1 bit to filter which 4 bits of the 8-bit word to read.

## Instructions

| Bits | Full Name | Mnemonic | Operation |
| ---- | --------- | -------- | --------- |
| 0000 | Store Next Value | SXV | `R1 := *(PC+1)` |
| 0001 | Accumulate Next Value | AXV | `R1 := R1 + *(PC+1)` |
| 0010 | Shift Bitwise Or Next Value | SON | `R1 := (R1 << 4) \| *(PC+1)` |
| 0011 | Copy | COP | `R2 := R1` |
| 0100 | Add | ADD | `R1 := R1 + R2` |
| 0101 | Subtract | SUB | `R1 := R1 - R2` |
| 0110 | Multiply | MUL | `R1 := R1 * R2` |
| 0111 | Divide | DIV | `R1 := R1 / R2` |
| 1000 | Jump to ROM Address in Next  | JRX | `PC := ROM[*(PC+1)]` |
| 1001 | Branch to ROM Address in Next if Zero | BRZ | `PC := ROM[*(PC+1)]` if `R1=0` |
| 1010 | Branch to ROM Address in Next if Negative | BRN | `PC := ROM[*(PC+1)]` if `R1<0` |
| 1011 | Branch to ROM Address in Next if Positive | BRP | `PC := ROM[*(PC+1)]` if `R1>0` |
| 1100 | Push to Stack | PUS | `*SP := R1; SP := SP - 1` |
| 1101 | Pop from Stack | POP | `SP := SP + 1; R1 := *SP` |
| 1110 | Read from stdin | RIN | `R1 := (next 8 bits from stdin)` (syscall) |
| 1111 | Output to stdout | OUT | `stdout << R1` (syscall) |
| -    | Halt             | HLT | Halts the CPU if three read instructions are `0000` |

**Clarification for `Halt`**: If the machine code (in hex) is `0 1 3 4 0 0 0 1 4 F` then the machine will halt before reading the last three instructions `1 4 F`.
Not including the `HLT` as an instructions in the code means it will halt when it reaches the end of the program if the memory space after the instructions are reset to 0.
# Assembly Language Grammar
Note that the the `<NullaryInst>`, `<NextValueInst>` and `<JumpBranchInst>` are case insensitive.
So for example `cop` `COP` and `CoP` all mean the *Copy Instruction*.

```html
    <Program>        ::= Epsilon
                       | <Statement> <Program>
    <Statement>      ::= <Label>
                       | <Inst>
    <Label>          ::= <Identifer> ":"
    <Inst>           ::= <NullaryInst>
                       | <NextValueInst> <Num>
                       | <JumpBranchInst> <Ident>
    
    <Ident>          ::= [_a-zA-z][a-zA-z0-9]*
    <Num>            ::= -?[0-7] | "-8" | <Hexit>
    <Hexit>          ::= 0x([0-9]|[a-f]|[A-F])
    <NullaryInst>    ::= "cop"
                       | "add"
                       | "sub"
                       | "mul"
                       | "div"
                       | "pus"
                       | "pop"
                       | "rin"
                       | "out"
    <NextValueInst>  ::= "sxv"
                       | "axv"
                       | "son"
    <JumpBranchInst> ::= "jrx"
                       | "brz"
                       | "brn"
                       | "brp"
```
