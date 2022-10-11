#!/bin/bash

gcc -p -g -DUSE_GC_OBJECT -o test-gc-object test_gc_object.c deserialize.c errors.c interpreter.c langtypes.c native.c opcodes.c util.c gc_object.c xmalloc.c -lm

