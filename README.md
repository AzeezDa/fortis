# Fortis - A 4-bit-Instruction-8-bit-Register-12-bit-addressing Computer

## Table of Content

- [Installation](#installation)
- [Usage](#usage)
- [Instruction Set Architecture](#instruction-set-architecture)
    - [Registers](#registers)
    - [Instructions](#instructions)
- [Assembly Language Grammar](#assembly-language-grammar)
- [Assembler and Machine Code File Structure](#assembler-and-machine-code-file-structure)
    - [ROM](#rom)
    - [Instructions](#instructions-1)
    - [Example](#example)

## Installation
Prerequisites: [Rust, Cargo](https://www.rust-lang.org/tools/install) and [GHC](https://www.haskell.org/ghc/download.html) installed.


Currently there is only installation for Linux.

**Step 1:** Clone this repository to somewhere you want (or by default `~/fortis`):

```
git clone https://github.com/AzeezDa/fortis.git ~/fortis
```

**Step 2:** Go into the directory where the repository was cloned 
```
cd ~/fortis 
```

**Step 3:** Give the `install` execute permission
```
chmod +x install
```

**Step 4:** Run the `install` script:
```
./install
```
If no errors occurred and the install printed (may differ a bit, but most important is the last line)
```
Compiling the Fortis Assembler
[1 of 4] Compiling Lexer            ( Lexer.hs, Lexer.o )
[2 of 4] Compiling Parser           ( Parser.hs, Parser.o )
[3 of 4] Compiling Assembler        ( Assembler.hs, Assembler.o )
[4 of 4] Compiling Main             ( Main.hs, Main.o )
Linking ../build/assembler ...
The Fortis Assembler compiled successfully!
Compiling the Fortis Machine Emulator
   Compiling emulator v0.1.0 (/home/name/fortis/emulator)
    Finished release [optimized] target(s) in 0.93s
The Fortis Machine Emulator compiled successfully!
Setting up the Fortis executable
Installation complete!
```
then everything is good to go!

**Step 6 (Optional):** Add the following to `.bashrc` or similar to alias the `fortis` executable
```
alias fortis="~/fortis/build/fortis"
```
## Usage
Run `~/build/fortis -h` (or `fortis -h` if you have aliased it) for help about using the assembler and emulator.
## Instruction Set Architecture

### Registers
| Register | Description |
| -------- | ----------- |
| R1 | 8-bit Register 1 |
| R2 | 8-bit Register 2 |
| SP | 12-bit Stack Pointer, starts at 0xFFF |
| PC | 12-bit Program Counter, starts at 0x000 |

Notes: The SP can span the entire memory space but the PC will use 11 bits for the memory access and 1 bit to filter which 4 bits of the 8-bit word to read.

### Instructions

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

## Assembly Language Grammar
Note that the the `<NullaryInst>`, `<NextValueInst>` and `<JumpBranchInst>` are case insensitive.
So for example `cop` `COP` and `CoP` all mean the *Copy Instruction*.

```html
    <Program>        ::= Epsilon
                       | <Statement> <Program>
    <Statement>      ::= <Label>
                       | <Inst>
    <Label>          ::= <Identifier> ":"
    <Inst>           ::= <NullaryInst>
                       | <NextValueInst> <Num>
                       | <JumpBranchInst> <Identifier>
    
    <Identifier>     ::= [_a-zA-z][a-zA-z0-9]*
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
                       | "hlt"
    <NextValueInst>  ::= "sxv"
                       | "axv"
                       | "son"
    <JumpBranchInst> ::= "jrx"
                       | "brz"
                       | "brn"
                       | "brp"
```

## Assembler and Machine Code File Structure
#### ROM
The assembler takes a Fortis Assembly Language file and generates the corresponding machine code file. The machine code contains both the ROM and the instructions.

The first 24 bytes contain the values of the 16 12-bit ROM memory cells. Where every 3 bytes contain 2 memory cells in this structure:

```
BYTE 0: [|r7 r6 r5 r4 r3 r2 r1 r0|]
BYTE 1: [|R3 R2 R1 R0|rB rA r9 r8|]
BYTE 2: [|RB RA R9 R8 R7 R6 R5 R4|]
```

Where `rx` is the first ROM cell and the `Rx` is the following ROM cell. (`x` is the bit index of each ROM cell).

So to unroll these three bytes we take the 0th byte and append the least significant nibble of the 1st byte unto the 0th byte. This is the zeroth ROM cell.
```
ROM 0: [|rB rA r9 r8 r7 r6 r5 r4 r3 r2 r1 r0|]
```
The second ROM cell is given by the taking the most significant nibble of the 1st byte and append the 2nd byte to it giving:
```
ROM 1: [|RB RA R9 R8 R7 R6 R5 R4 R3 R2 R1 R0|]
```
This is continued until all 24 bytes are converted.

#### Instructions
After the first 24 bytes, the instructions are stored where each byte contains two instructions. The least significant nibble is the first instruction, and the most significant nibble is the following instruction. Thus the instructions have the following byte-wise structure:
```
BYTE 0: [|I1 I1 I1 I1|I0 I0 I0 I0|]
BYTE 1: [|I3 I3 I3 I3|I2 I2 I2 I2|]
BYTE 2: [|I5 I5 I5 I5|I6 I6 I6 I6|]
 . . .          .    .    .
```
Where `Ix` are the bits of instruction `x` (do not confuse it with the ROM description above).

Therefore, to extract the `x`th instruction, extract the `(x>>2)`th byte then if `(x&1)` is `0` then we instruction we seek the is the least significant nibble of the extracted byte, otherwise we extract the most significant nibble of that byte. 

#### Example
Assembling the [factorial.asm](examples/factorial.asm) file will produce the following file (which is hexdumped).

```
$ hexdump -e '"%04.4_ax: " 24/1 "%02x " "\n"' out
0000:  2d 80 03 3c 20 08 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
0018:  40 e2 0f 23 ff 20 02 0f 23 30 5e 19 3a cc 50 d3 b5 d3 0c fa 10 d3 6c d3
0030:  f1 bc 40 28 10 28 0c 26 34 cd c7 0c 23 30 4d 0f 26 34 6d d3 c5 a0 d3 7c
0048:  cc 30 02 d3 f4 a0 d3 36 cd c5 30 02 d3 f4 a0 0f 00 70 52 1f f9 51 1f f2
0060:  b1 ff f1 1f f3 21 0f 26 f5 f1 0f fa 00 00  
```