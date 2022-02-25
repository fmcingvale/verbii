#!/bin/sh
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values

os=`uname -o`

if [ $os = "Msys" ];
then
	REPL="./repl.exe"
	UTEST="./test.exe"
else
	REPL="./repl"
	UTEST="./test"
fi

$REPL -test ../unittests/unittest_core.verb > ../unittests/RESULTS/c++/actual_unittest_core.txt
$REPL -test ../unittests/unittest_basic.verb > ../unittests/RESULTS/c++/actual_unittest_basic.txt
$REPL -test ../unittests/unittest_errors.verb > ../unittests/RESULTS/c++/actual_unittest_errors.txt

# demos are complete programs, so no -test
$REPL ../unittests/demo_math.verb > ../unittests/RESULTS/c++/actual_demo_math.txt

