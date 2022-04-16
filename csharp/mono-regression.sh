#!/bin/bash
#
# same as regression.bat but assumes running with mono instead of dotnet
#
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values

mkdir -p ../unittests/RESULTS/csharp

echo "Core ..."
./repl.exe -test ../unittests/unittest_core.verb > ../unittests/RESULTS/csharp/actual_unittest_core.txt
echo "Basic ..."
./repl.exe -test ../unittests/unittest_basic.verb > ../unittests/RESULTS/csharp/actual_unittest_basic.txt
echo "Errors ..."
./repl.exe -test ../unittests/unittest_errors.verb > ../unittests/RESULTS/csharp/actual_unittest_errors.txt

# demos are full programs, so run without -test
echo "Demo ..."
./repl.exe ../unittests/unittest_demo.verb > ../unittests/RESULTS/csharp/actual_unittest_demo.txt

# not a formal test yet
echo "Math ..."
./repl.exe ../unittests/demo_math.verb > ../unittests/RESULTS/csharp/actual_demo_math.txt
