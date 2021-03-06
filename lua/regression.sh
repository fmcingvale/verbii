#!/bin/sh
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values

mkdir -p ../unittests/RESULTS/lua

# use -nocache here like other ports

echo "Core ..."
./verbii -test -nocache ../unittests/unittest_core.verb > ../unittests/RESULTS/lua/actual_unittest_core.txt
echo "Basic ..."
./verbii -test -nocache ../unittests/unittest_basic.verb > ../unittests/RESULTS/lua/actual_unittest_basic.txt
echo "Errors ..."
./verbii -test -nocache ../unittests/unittest_errors.verb > ../unittests/RESULTS/lua/actual_unittest_errors.txt

# demos are full programs, so run without -test
echo "Demo ..."
./verbii -nocache ../unittests/unittest_demo.verb > ../unittests/RESULTS/lua/actual_unittest_demo.txt

echo "Math ..."
./verbii -nocache ../unittests/demo_math.verb > ../unittests/RESULTS/lua/actual_demo_math.txt
