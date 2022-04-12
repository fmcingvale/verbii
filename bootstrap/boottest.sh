#!/bin/bash
#
# another sanity check that all ports compile init.verb and compiler.verb identically.

echo "There should be no output below this line, except for port names:"

mkdir -p test-output
rm -f test-output/*

COMPILER="../verb/compile.verb"

# treat the c++ port as the reference
echo "C++ ..."
../c++/repl $COMPILER -- ../lib/init.verb > test-output/init-c-CPP.txt
../c++/repl $COMPILER -- ../lib/compiler.verb > test-output/compiler-c-CPP.txt

# now test others against it

echo "Python ..."
# check with python port running compiler.verb
python ../new-python/repl.py $COMPILER -- ../lib/init.verb > test-output/init-c-PY.txt
python ../new-python/repl.py $COMPILER -- ../lib/compiler.verb > test-output/compiler-c-PY.txt

diff -u -b -B test-output/init-c-PY.txt test-output/init-c-CPP.txt
diff -u -b -B test-output/compiler-c-PY.txt test-output/compiler-c-CPP.txt

echo "Lua ..."
# check with lua port running compiler.verb
( cd ../lua ; lua repl.lua $COMPILER -- ../lib/init.verb > ../bootstrap/test-output/init-c-LUA.txt )
( cd ../lua ; lua repl.lua $COMPILER -- ../lib/compiler.verb > ../bootstrap/test-output/compiler-c-LUA.txt )

diff -u -b -B test-output/init-c-LUA.txt test-output/init-c-CPP.txt
diff -u -b -B test-output/compiler-c-LUA.txt test-output/compiler-c-CPP.txt

echo "C# ..."
# check with c# port
../csharp/repl.exe $COMPILER -- ../lib/init.verb > test-output/init-c-CSHARP.txt
../csharp/repl.exe $COMPILER -- ../lib/compiler.verb > test-output/compiler-c-CSHARP.txt

diff -u -b -B test-output/init-c-CSHARP.txt test-output/init-c-CPP.txt
diff -u -b -B test-output/compiler-c-CSHARP.txt test-output/compiler-c-CPP.txt

echo "Chicken ..."
# check with chicken port
../chicken/repl $COMPILER -- ../lib/init.verb > test-output/init-c-CHICKEN.txt
../chicken/repl $COMPILER -- ../lib/compiler.verb > test-output/compiler-c-CHICKEN.txt

diff -u -b -B test-output/init-c-CHICKEN.txt test-output/init-c-CPP.txt
diff -u -b -B test-output/compiler-c-CHICKEN.txt test-output/compiler-c-CPP.txt

