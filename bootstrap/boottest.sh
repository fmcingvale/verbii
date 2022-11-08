#!/bin/bash
#
# another sanity check that all ports compile boot/init/compiler.verb identically.
#
# Copyright (c) 2022 Frank McIngvale, see LICENSE

echo "There should be no output below this line, except for port names:"

mkdir -p test-output
rm -f test-output/*

# path to verbii compiler script
COMPILER="../verb/compile.verb"

# set VERBII_BOOT since this should always run from this location.
# this is convenient since it doesn't require it to be preset as well
# as allowing the C# tests to run under WSL which for some reason
# cannot handle absolute paths from /mnt ...
export VERBII_BOOT=../lib/

# treat the C port as the reference
echo "C ..."
../cpp/verbii $COMPILER -- ../lib/boot.verb > test-output/boot-c-C.txt
../cpp/verbii $COMPILER -- ../lib/init.verb > test-output/init-c-C.txt
../cpp/verbii $COMPILER -- ../lib/compiler.verb > test-output/compiler-c-C.txt

# now test others against it

echo "C++ ..."
../cpp/verbii $COMPILER -- ../lib/boot.verb > test-output/boot-c-CPP.txt
../cpp/verbii $COMPILER -- ../lib/init.verb > test-output/init-c-CPP.txt
../cpp/verbii $COMPILER -- ../lib/compiler.verb > test-output/compiler-c-CPP.txt

diff -u -b -B test-output/boot-c-CPP.txt test-output/boot-c-C.txt
diff -u -b -B test-output/init-c-CPP.txt test-output/init-c-C.txt
diff -u -b -B test-output/compiler-c-CPP.txt test-output/compiler-c-C.txt

echo "Python ..."
# check with python port running compiler.verb
../python/verbii $COMPILER -- ../lib/boot.verb > ../bootstrap/test-output/boot-c-PY.txt
../python/verbii $COMPILER -- ../lib/init.verb > ../bootstrap/test-output/init-c-PY.txt
../python/verbii $COMPILER -- ../lib/compiler.verb > ../bootstrap/test-output/compiler-c-PY.txt

diff -u -b -B test-output/boot-c-PY.txt test-output/boot-c-C.txt
diff -u -b -B test-output/init-c-PY.txt test-output/init-c-C.txt
diff -u -b -B test-output/compiler-c-PY.txt test-output/compiler-c-C.txt

echo "Lua ..."
# check with lua port running compiler.verb
../lua/verbii $COMPILER -- ../lib/boot.verb > ../bootstrap/test-output/boot-c-LUA.txt
../lua/verbii $COMPILER -- ../lib/init.verb > ../bootstrap/test-output/init-c-LUA.txt
../lua/verbii $COMPILER -- ../lib/compiler.verb > ../bootstrap/test-output/compiler-c-LUA.txt

diff -u -b -B test-output/boot-c-LUA.txt test-output/boot-c-C.txt
diff -u -b -B test-output/init-c-LUA.txt test-output/init-c-C.txt
diff -u -b -B test-output/compiler-c-LUA.txt test-output/compiler-c-C.txt

echo "C# ..."
# check with c# port
../csharp/verbii $COMPILER -- ../lib/boot.verb > test-output/boot-c-CSHARP.txt
../csharp/verbii $COMPILER -- ../lib/init.verb > test-output/init-c-CSHARP.txt
../csharp/verbii $COMPILER -- ../lib/compiler.verb > test-output/compiler-c-CSHARP.txt

diff -u -b -B test-output/boot-c-CSHARP.txt test-output/boot-c-C.txt
diff -u -b -B test-output/init-c-CSHARP.txt test-output/init-c-C.txt
diff -u -b -B test-output/compiler-c-CSHARP.txt test-output/compiler-c-C.txt

echo "Chicken ..."
# check with chicken port
../chicken/verbii $COMPILER -- ../lib/boot.verb > test-output/boot-c-CHICKEN.txt
../chicken/verbii $COMPILER -- ../lib/init.verb > test-output/init-c-CHICKEN.txt
../chicken/verbii $COMPILER -- ../lib/compiler.verb > test-output/compiler-c-CHICKEN.txt

diff -u -b -B test-output/boot-c-CHICKEN.txt test-output/boot-c-C.txt
diff -u -b -B test-output/init-c-CHICKEN.txt test-output/init-c-C.txt
diff -u -b -B test-output/compiler-c-CHICKEN.txt test-output/compiler-c-C.txt

