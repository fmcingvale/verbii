#!/bin/sh
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values

mkdir -p ../unittests/RESULTS/chicken

# choose whether to run interpreter or compiled version
#CMD="./local-chicken/bin/csi -s verbii.scm"

CMD="./repl"
make

echo "Core ..."
$CMD -test ../unittests/unittest_core.verb > ../unittests/RESULTS/chicken/actual_unittest_core.txt
echo "Basic ..."
$CMD -test ../unittests/unittest_basic.verb > ../unittests/RESULTS/chicken/actual_unittest_basic.txt
echo "Errors ..."
$CMD -test ../unittests/unittest_errors.verb > ../unittests/RESULTS/chicken/actual_unittest_errors.txt

# demos are full programs, so run without -test
echo "Demo ..."
$CMD ../unittests/unittest_demo.verb > ../unittests/RESULTS/chicken/actual_unittest_demo.txt

echo "Math ..."
$CMD ../unittests/demo_math.verb > ../unittests/RESULTS/chicken/actual_demo_math.txt
