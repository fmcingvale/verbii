#!/bin/sh
#
# Setup build for linux
rm -rf bin/

# uncomment one of the following build types ...

# the default is to use Boehm GC, if it is found - else use the custom garbage collector.
# (the custom GC is quite a bit slower than Boehm and uses more memory as well,
# but if Boehm isn't available, it's better than nothing)
cmake -DCMAKE_BUILD_TYPE=Release -S src/ -B bin

# to build WITHOUT garbage collection, set USE_NO_GC
#
# this can be useful when tracking down weird bugs to turn off all gc
#cmake -DCMAKE_BUILD_TYPE=Release -DUSE_NO_GC=1 -S src/ -B bin

# to use the custom GC even if Boehm is available, use this one ...
#cmake -DCMAKE_BUILD_TYPE=Release -DUSE_GC_OBJECT=1 -S src/ -B bin

# to help find memory leaks in the custom GC, use this one ...
#cmake -DCMAKE_BUILD_TYPE=Release -DUSE_GC_OBJECT=1 -DUSE_XMEM_TRACE=1 -S src/ -B bin
