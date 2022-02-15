#!/bin/bash
#
# same as regression.bat but assumes running with mono instead of dotnet
#
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values
./repl.exe -test ../unittests/unittest_core.txt > ../unittests/RESULTS/csharp/actual_unittest_core.txt
./repl.exe -test ../unittests/unittest_basic.txt > ../unittests/RESULTS/csharp/actual_unittest_basic.txt
./repl.exe -test ../unittests/unittest_errors.txt > ../unittests/RESULTS/csharp/actual_unittest_errors.txt

# demos are full programs, so run without -test
./repl.exe ../unittests/demo_math.txt > ../unittests/RESULTS/csharp/actual_demo_math.txt
