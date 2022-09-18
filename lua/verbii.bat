@echo off
setlocal
set LUA_PATH=%~dp0?.lua 
lua %~dp0main.lua -libdir %VERBII_BOOT%\lib\ %*
endlocal

