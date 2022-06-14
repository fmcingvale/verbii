#!/bin/sh
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values

mkdir -p ../unittests/RESULTS/lua

echo "Core ..."
./verbii -test ../unittests/unittest_core.verb > ../unittests/RESULTS/lua/actual_unittest_core.txt
echo "Basic ..."
./verbii -test ../unittests/unittest_basic.verb > ../unittests/RESULTS/lua/actual_unittest_basic.txt
echo "Errors ..."
./verbii -test ../unittests/unittest_errors.verb > ../unittests/RESULTS/lua/actual_unittest_errors.txt

# demos are full programs, so run without -test
echo "Demo ..."
./verbii ../unittests/unittest_demo.verb > ../unittests/RESULTS/lua/actual_unittest_demo.txt

echo "Math ..."
./verbii ../unittests/demo_math.verb > ../unittests/RESULTS/lua/actual_demo_math.txt
