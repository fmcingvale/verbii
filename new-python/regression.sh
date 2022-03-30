#!/bin/sh
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values

mkdir -p ../unittests/RESULTS/python

echo "Core ..."
python repl.py -test ../unittests/unittest_core.verb > ../unittests/RESULTS/python/actual_unittest_core.txt
echo "Basic ..."
python repl.py -test ../unittests/unittest_basic.verb > ../unittests/RESULTS/python/actual_unittest_basic.txt
echo "Errors ..."
python repl.py -test ../unittests/unittest_errors.verb > ../unittests/RESULTS/python/actual_unittest_errors.txt

# demos are full programs, so run without -test
echo "Math ..."
python repl.py ../unittests/demo_math.verb > ../unittests/RESULTS/python/actual_demo_math.txt
