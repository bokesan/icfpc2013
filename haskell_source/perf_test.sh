#!/bin/sh
echo "expected result: 24040193"
time ./Main gen 11 if0,fold,and,or,xor,plus,not,shl1,shr1,shr4,shr16|wc -l
echo
echo "Core i7-4771:              50.4 sec user"
echo "AMD Phenom X6 1090T:      104.9 sec user"
echo "AMD A10-7850K:            108.7 sec user"

