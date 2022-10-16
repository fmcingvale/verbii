#!/bin/sh
#
# Setup build for mingw64
rm -rf bin

# uncomment one of the following build types ...

# to build WITHOUT garbage collection, set USE_NO_GC
#
# this can be useful when tracking down weird bugs to turn off all gc
#cmake -G "MSYS Makefiles" -DCMAKE_BUILD_TYPE=Release -DUSE_NO_GC=1 -S src/ -B bin

# to build WITH gc-object garbage collection
#cmake -G "MSYS Makefiles" -DCMAKE_BUILD_TYPE=Release -DUSE_GC_OBJECT=1 -S src/ -B bin

# build with Boehm GC (if found), else currently defaults to USE_NO_GC
# (eventually need to change this to default to USE_GC_OBJECT once it is stable)
cmake -G "MSYS Makefiles" -DCMAKE_BUILD_TYPE=Release -S src/ -B bin

