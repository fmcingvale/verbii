#!/bin/bash
#
# Built debugging version with no GC and only works on posix/gcc
#gcc -p -g -o main main.c deserialize.c errors.c interpreter.c langtypes.c native.c opcodes.c util.c gc_object.c xmalloc.c -lm

# Build debugging version with gc-object
gcc -p -g -DUSE_GC_OBJECT -o main main.c deserialize.c errors.c interpreter.c langtypes.c native.c opcodes.c util.c gc_object.c xmalloc.c -lm

cp main ..
