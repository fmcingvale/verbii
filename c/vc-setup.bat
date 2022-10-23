@echo off
REM Setup build for Visual Studio
REM 
REM Tested with Visual Studio 2022 Community Edition with CMake installed
REM 
REM Start a developer command shell, e.g.:
REM 	Start -> Visual Studio 2022 -> Developer Command Prompt for VS 2022
REM 
REM Run vc-setup.bat followed by vc-make.bat

REM uncomment one of the following build types ...

REM NOTE: The '-G "Ninja"' below allows the build type to be specified with -DCMAKE_BUILD_TYPE
REM just like the mingw/linux builds. Without this it appears to only build the debug version.

REM the default is to use Boehm GC, if it is found - else use the custom garbage collector.
REM (the custom GC is quite a bit slower than Boehm and uses more memory as well,
REM but if Boehm isn't available, it's better than nothing)
REM cmake -DCMAKE_BUILD_TYPE=Release -S src/ -B bin
cmake -G "Ninja" -DCMAKE_BUILD_TYPE=Release -S src/ -B bin

REM to build WITHOUT garbage collection, set USE_NO_GC
REM
REM this can be useful when tracking down weird bugs to turn off all gc
REM cmake -G "Ninja" -DCMAKE_BUILD_TYPE=Release -DUSE_NO_GC=1 -S src/ -B bin

REM to use the custom GC even if Boehm is available, use this one ...
REM cmake -G "Ninja" -DCMAKE_BUILD_TYPE=Release -DUSE_GC_OBJECT=1 -S src/ -B bin

REM to help find memory leaks in the custom GC, use this one ...
REM cmake -G "Ninja" -DCMAKE_BUILD_TYPE=Release -DUSE_GC_OBJECT=1 -DUSE_XMEM_TRACE=1 -S src/ -B bin

