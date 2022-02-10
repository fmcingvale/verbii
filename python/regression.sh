#!/bin/sh
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values
python repl.py -test ../unittests/unittest_core.txt > ../unittests/RESULTS/python/actual_unittest_core.txt
python repl.py -test ../unittests/unittest_basic.txt > ../unittests/RESULTS/python/actual_unittest_basic.txt
python repl.py -test ../unittests/unittest_errors.txt > ../unittests/RESULTS/python/actual_unittest_errors.txt
