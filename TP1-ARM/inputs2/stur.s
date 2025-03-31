.text
mov X1, 0x1000
lsl X1, X1, 16
mov X10, 0x1234
stur X10, [X1, 0x1]
stur X10, [X1, 0x3]
stur X10, [X1, 0x6]
mov X2, 0x1000
lsl X3, X2, 16
adds X3, X3, 111
lsl X10, X10, 10
stur X10, [X3, 0x0]
HLT 0

