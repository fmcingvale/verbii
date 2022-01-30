#!/bin/sh
# run this before checking in revisions so i can see if the unittest results
# changed from the expected values
./repl.exe -test unittest_core.txt > expect_unittest_core.txt
./repl.exe -test unittest_basic.txt > expect_unittest_basic.txt
