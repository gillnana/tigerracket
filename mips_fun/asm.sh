#!/bin/bash


mips-linux-gcc -S -O0 -mips1 -mno-abicalls -nostdlib -o libtiger.s libtiger.c 
 
mips-linux-gcc -c -O0 -mips1 -mno-abicalls -nostdlib -o libtiger.o libtiger.c  
mips-linux-as -O0 -mips1 -o "$1.o" "$1.s"
mips-linux-ld -o "$1" "$1.o" libtiger.o /course/cs195r/lib/mips-rt/mips-rt.a  
qemu-mips -d in_asm "$1"









