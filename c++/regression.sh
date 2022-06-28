#!/bin/sh
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values

os=`uname -o`

if [ $os = "Msys" ];
then
	REPL="./verbii.exe"
else
	REPL="./verbii"
fi

# sure binaries up to date
make

# run tests of stable version
mkdir -p ../unittests/RESULTS/c++

echo "*** Running with $REPL ..."

# use -nocache since that gives a more stressful test and avoid issues with e.g. tests crashing and
# leaving corrupted .b files

echo "Core ..."
$REPL -test -nocache ../unittests/unittest_core.verb > ../unittests/RESULTS/c++/actual_unittest_core.txt
echo "Basic ..."
$REPL -test -nocache ../unittests/unittest_basic.verb > ../unittests/RESULTS/c++/actual_unittest_basic.txt
echo "Errors ..."
$REPL -test -nocache ../unittests/unittest_errors.verb > ../unittests/RESULTS/c++/actual_unittest_errors.txt

# demos are complete programs, so no -test
echo "Demo ..."
$REPL -nocache ../unittests/unittest_demo.verb > ../unittests/RESULTS/c++/actual_unittest_demo.txt

echo "Math ..."
$REPL -nocache ../unittests/demo_math.verb > ../unittests/RESULTS/c++/actual_demo_math.txt
