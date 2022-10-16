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

REM to build WITHOUT garbage collection, set USE_NO_GC
REM
REM this can be useful when tracking down weird bugs to turn off all gc
REM cmake -DCMAKE_BUILD_TYPE=Release -DUSE_NO_GC=1 -S src/ -B bin

REM to build WITH gc-object garbage collection
cmake -DCMAKE_BUILD_TYPE=Release -DUSE_GC_OBJECT=1 -S src/ -B bin

REM build with memory tracing with gc-object (for debugging memory leaks)
REM cmake -DCMAKE_BUILD_TYPE=Release -DUSE_GC_OBJECT=1 -DUSE_XMEM_TRACE=1 -S src/ -B bin

REM build with Boehm GC (if found), else currently defaults to USE_NO_GC
REM  (eventually need to change this to default to USE_GC_OBJECT once it is stable)
REM cmake -DCMAKE_BUILD_TYPE=Release -S src/ -B bin

