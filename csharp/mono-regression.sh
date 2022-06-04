#!/bin/bash
#
# same as regression.bat but assumes running with mono instead of dotnet
#
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values

# ensure build is up to date
./mono-build.sh

mkdir -p ../unittests/RESULTS/csharp

echo "* STABLE ..."

echo "Core ..."
./repl.exe -test ../unittests/unittest_core.verb > ../unittests/RESULTS/csharp/actual_unittest_core.txt
echo "Basic ..."
./repl.exe -test ../unittests/unittest_basic.verb > ../unittests/RESULTS/csharp/actual_unittest_basic.txt
echo "Errors ..."
./repl.exe -test ../unittests/unittest_errors.verb > ../unittests/RESULTS/csharp/actual_unittest_errors.txt

# demos are full programs, so run without -test
echo "Demo ..."
./repl.exe ../unittests/unittest_demo.verb > ../unittests/RESULTS/csharp/actual_unittest_demo.txt

echo "Math ..."
./repl.exe ../unittests/demo_math.verb > ../unittests/RESULTS/csharp/actual_demo_math.txt

mkdir -p ../unittests/RESULTS/mini-csharp

echo "* EXPERIMENTAL ..."

echo "Core ..."
./min.exe -test ../unittests/unittest_core.verb > ../unittests/RESULTS/mini-csharp/actual_unittest_core.txt
echo "Basic ..."
./min.exe -test ../unittests/unittest_basic.verb > ../unittests/RESULTS/mini-csharp/actual_unittest_basic.txt
echo "Errors ..."
./min.exe -test ../unittests/unittest_errors.verb > ../unittests/RESULTS/mini-csharp/actual_unittest_errors.txt

# demos are full programs, so run without -test
echo "Demo ..."
./min.exe ../unittests/unittest_demo.verb > ../unittests/RESULTS/mini-csharp/actual_unittest_demo.txt

echo "Math ..."
./min.exe ../unittests/demo_math.verb > ../unittests/RESULTS/mini-csharp/actual_demo_math.txt
