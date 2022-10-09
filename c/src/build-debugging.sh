#!/bin/bash
#
# Built debugging version with no GC and only works on posix/gcc
gcc -p -g -o main main.c deserialize.c errors.c interpreter.c langtypes.c native.c opcodes.c util.c xmalloc.c -lm
cp main ..
