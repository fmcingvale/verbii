#!/bin/bash

# General note: Even though verbii is designed to produce identical (or nearly identical)
# output across platforms, tests results cannot be checked by diffing output versus
# an expected output. Reasons:
#
#	1. Rounding errors in floats means printed floats won't always match exactly.
#	2. 'keys' function is not required to return keys in sorted order, so any tests
#		that check or use the output of keys may have different ordered results
#		across platforms.
#
# HOWEVER, the test suite has been design to be self-checking so that textual
# comparison is not required to ensure a correct result. The real check is to
# look at the # of tests passed/failed and make sure they match across platforms.

# force all files to be recompiled at least once
rm -f *.verb.b

# remove old outputs
rm TEST-RESULTS/output-*.txt

# set VERBII_BOOT since this should always run from this location.
# this is convenient since it doesn't require it to be preset as well
# as allowing the C# tests to run under WSL which for some reason
# cannot handle absolute paths from /mnt ...
export VERBII_BOOT=../lib/

echo "C ..."
../c/verbii run-all-tests.verb > TEST-RESULTS/output-c.txt
echo "C++ ..."
../cpp/verbii run-all-tests.verb > TEST-RESULTS/output-cpp.txt
echo "C# ..."
../csharp/verbii run-all-tests.verb > TEST-RESULTS/output-csharp.txt
echo "Python ..."
../python/verbii run-all-tests.verb > TEST-RESULTS/output-python.txt
echo "Chicken ..."
../chicken/verbii run-all-tests.verb > TEST-RESULTS/output-chicken.txt
echo "Lua ..."
../lua/verbii run-all-tests.verb > TEST-RESULTS/output-lua.txt

echo "** Make sure ALL pass/fail numbers match below ..."
echo "** (Should be 1 pass and 1 fail line for each port listed above)"
echo " "
grep -h "Tests passed:" TEST-RESULTS/output-*.txt
echo " "
grep -h "Tests failed:" TEST-RESULTS/output-*.txt
