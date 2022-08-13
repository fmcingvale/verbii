#!/bin/sh
#
# Setup build for mingw64
rm -rf bin

# NOTE: The MinSizeRel is so cmake will use -Os instead of -O3 -- -O3 causes
# an immediate segfault under mingw currently (g++ 12.1.0)
cmake -G "MSYS Makefiles" -DCMAKE_BUILD_TYPE=MinSizeRel -S src -B bin

