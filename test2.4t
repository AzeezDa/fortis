SXV 0x4 SON 0xE OUT # N
SXV 0x3 SON 0xF OUT # ?
SXV 0x2 SON 0x0 OUT # <Space>

SXV 3 SON 0 COP     # R2 = 0x30
RIN SUB             # R1 = N

# Checking bounds
# N = 0
BRZ zero
# N < 0
BRN unsupported
# N > 5
PUS PUS SXV 5 COP
POP SUB
BRP unsupported
POP

PUS SXV 0xA OUT
SXV 1 COP           # R2 = 1
POP PUS             # R1 = N

loop:
MUL COP             # R2 *= R1
POP AXV -1 PUS      # R1 -= 1
BRP loop
ADD                 # R1 = N!
JRX print

zero: SXV 1 JRX print

# === Printing ===
print:
PUS
SXV 0x6 SON 0x4 COP # R2 = 100
POP PUS DIV PUS     # R1 = N! / 100
PUS SXV 0x3 SON 0x0
COP POP ADD OUT     # Print 1st digit

SXV 0x6 SON 0x4 COP # R2 = 100
POP MUL COP POP     # R2 = 100 * 1st digit
SUB PUS             # R1 = N! - R2

SXV 0xA COP         # R2 = 10
POP PUS DIV PUS     # R1 /= R1
PUS SXV 0x3 SON 0x0
COP POP ADD OUT     # Print 2nd digit

SXV 0xA COP         # R2 = 10
POP MUL COP POP PUS # R2 = 10 * 2nd digit
SUB PUS             # R1 = 3rd digit

SXV 0x3 SON 0x0 COP
POP ADD OUT         # Print 3rd digit
SXV 0xA OUT         # New Line
HLT

# == Print Unsupported ==
unsupported:
SXV 0x7 SON 0x5 OUT # u
AXV -7          OUT # n
AXV 5           OUT # s
AXV 2           OUT # u
AXV -5          OUT # p
                OUT # p
AXV -1          OUT # o
AXV 3           OUT # r
AXV 2           OUT # t
SXV 0x6 SON 0x5 OUT # e
AXV -1          OUT # d
SXV 0xA         OUT # <New Line>
HLT