#!/bin/sh
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values
./repl.exe -test ../unittests/unittest_core.txt > ../unittests/RESULTS/c++/expect_unittest_core.txt
./repl.exe -test ../unittests/unittest_basic.txt > ../unittests/RESULTS/c++/expect_unittest_basic.txt
./repl.exe -test ../unittests/unittest_errors.txt > ../unittests/RESULTS/c++/expect_unittest_errors.txt
# run c++ unittests too
./test.exe