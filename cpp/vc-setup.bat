REM Setup build for msvc++
REM 
REM To build, open Visual Studio - open this folder (do not have to open a project).
REM Answer "no" if asked about enabling CMake support.
REM Run Tools -> Command Line -> Developer Command prompt
REM Then run this batch file, followed by vcmake.bat
cmake -DCMAKE_BUILD_TYPE=Release -S src/ -B bin

