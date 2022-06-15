#!/bin/bash
#
# another sanity check that all ports compile init.verb and compiler.verb identically.

echo "There should be no output below this line, except for port names:"

mkdir -p test-output
rm -f test-output/*

# path to verbii compiler script
COMPILER="../verb/compile.verb"

# treat the c++ port as the reference
echo "C++ ..."
../c++/verbii $COMPILER -- ../lib/boot.verb > test-output/boot-c-CPP.txt
../c++/verbii $COMPILER -- ../lib/init.verb > test-output/init-c-CPP.txt
../c++/verbii $COMPILER -- ../lib/compiler.verb > test-output/compiler-c-CPP.txt

# now test others against it

echo "Python ..."
# check with python port running compiler.verb
( cd ../python; ./verbii $COMPILER -- ../lib/boot.verb > ../bootstrap/test-output/boot-c-PY.txt )
( cd ../python; ./verbii $COMPILER -- ../lib/init.verb > ../bootstrap/test-output/init-c-PY.txt )
( cd ../python; ./verbii $COMPILER -- ../lib/compiler.verb > ../bootstrap/test-output/compiler-c-PY.txt )

diff -u -b -B test-output/boot-c-PY.txt test-output/boot-c-CPP.txt
diff -u -b -B test-output/init-c-PY.txt test-output/init-c-CPP.txt
diff -u -b -B test-output/compiler-c-PY.txt test-output/compiler-c-CPP.txt

echo "Lua ..."
# check with lua port running compiler.verb
( cd ../lua ; ./verbii $COMPILER -- ../lib/boot.verb > ../bootstrap/test-output/boot-c-LUA.txt )
( cd ../lua ; ./verbii $COMPILER -- ../lib/init.verb > ../bootstrap/test-output/init-c-LUA.txt )
( cd ../lua ; ./verbii $COMPILER -- ../lib/compiler.verb > ../bootstrap/test-output/compiler-c-LUA.txt )

diff -u -b -B test-output/boot-c-LUA.txt test-output/boot-c-CPP.txt
diff -u -b -B test-output/init-c-LUA.txt test-output/init-c-CPP.txt
diff -u -b -B test-output/compiler-c-LUA.txt test-output/compiler-c-CPP.txt

echo "C# ..."
# check with c# port
../csharp/verbii $COMPILER -- ../lib/boot.verb > test-output/boot-c-CSHARP.txt
../csharp/verbii $COMPILER -- ../lib/init.verb > test-output/init-c-CSHARP.txt
../csharp/verbii $COMPILER -- ../lib/compiler.verb > test-output/compiler-c-CSHARP.txt

diff -u -b -B test-output/boot-c-CSHARP.txt test-output/boot-c-CPP.txt
diff -u -b -B test-output/init-c-CSHARP.txt test-output/init-c-CPP.txt
diff -u -b -B test-output/compiler-c-CSHARP.txt test-output/compiler-c-CPP.txt

echo "Chicken ..."
# check with chicken port
../chicken/verbii $COMPILER -- ../lib/boot.verb > test-output/boot-c-CHICKEN.txt
../chicken/verbii $COMPILER -- ../lib/init.verb > test-output/init-c-CHICKEN.txt
../chicken/verbii $COMPILER -- ../lib/compiler.verb > test-output/compiler-c-CHICKEN.txt

diff -u -b -B test-output/boot-c-CHICKEN.txt test-output/boot-c-CPP.txt
diff -u -b -B test-output/init-c-CHICKEN.txt test-output/init-c-CPP.txt
diff -u -b -B test-output/compiler-c-CHICKEN.txt test-output/compiler-c-CPP.txt

