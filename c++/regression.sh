#!/bin/sh
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values

os=`uname -o`

if [ $os = "Msys" ];
then
	REPL="./verbii.exe"
else
	# comment/uncomment one to switch between old repl & new frontend
	#REPL="./repl"
	REPL="./verbii"
fi

# sure binaries up to date
make

# run tests of stable version
mkdir -p ../unittests/RESULTS/c++

echo "*** Running with $REPL ..."

echo "Core ..."
$REPL -test ../unittests/unittest_core.verb > ../unittests/RESULTS/c++/actual_unittest_core.txt
echo "Basic ..."
$REPL -test ../unittests/unittest_basic.verb > ../unittests/RESULTS/c++/actual_unittest_basic.txt
echo "Errors ..."
$REPL -test ../unittests/unittest_errors.verb > ../unittests/RESULTS/c++/actual_unittest_errors.txt

# demos are complete programs, so no -test
echo "Demo ..."
$REPL ../unittests/unittest_demo.verb > ../unittests/RESULTS/c++/actual_unittest_demo.txt

echo "Math ..."
$REPL ../unittests/demo_math.verb > ../unittests/RESULTS/c++/actual_demo_math.txt
