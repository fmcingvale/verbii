@echo off
REM Setup build for Visual Studio
REM 
REM Tested with Visual Studio 2022 Community Edition with CMake installed
REM 
REM Start a developer command shell, e.g.:
REM 	Start -> Visual Studio 2022 -> Developer Command Prompt for VS 2022
REM 
REM Run vc-setup.bat followed by vc-make.bat
cmake -DCMAKE_BUILD_TYPE=Release -S src/ -B bin

