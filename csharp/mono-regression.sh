#!/bin/bash
#
# same as regression.bat but assumes running with mono instead of dotnet
#
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values

# ensure build is up to date
./mono-build.sh

mkdir -p ../unittests/RESULTS/csharp

# use -nocache since that gives a more stressful test and avoid issues with e.g. tests crashing and
# leaving corrupted .b files

echo "Core ..."
./verbii -test -nocache ../unittests/unittest_core.verb > ../unittests/RESULTS/csharp/actual_unittest_core.txt
echo "Basic ..."
./verbii -test -nocache ../unittests/unittest_basic.verb > ../unittests/RESULTS/csharp/actual_unittest_basic.txt
echo "Errors ..."
./verbii -test -nocache ../unittests/unittest_errors.verb > ../unittests/RESULTS/csharp/actual_unittest_errors.txt

# demos are full programs, so run without -test
echo "Demo ..."
./verbii -nocache ../unittests/unittest_demo.verb > ../unittests/RESULTS/csharp/actual_unittest_demo.txt

echo "Math ..."
./verbii -nocache ../unittests/demo_math.verb > ../unittests/RESULTS/csharp/actual_demo_math.txt
