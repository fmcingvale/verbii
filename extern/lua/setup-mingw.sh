#!/bin/sh
#
# Setup build for mingw64
rm -rf bin

cmake -G "MSYS Makefiles" -DCMAKE_BUILD_TYPE=Release -S src -B bin

