#!/bin/bash
#
# test that bootstrap compiler produces identical output to compiler.verb,
# on init.verb and compiler.verb
#
# should run this as a sanity check before running make-bootstrap.sh

echo "There should be no output below this line, except for port names:"

mkdir -p test-output
rm -f 	test-output/*

echo "Bootstrap ..."
# use python bootstrap compiler first
python compiler.py ../lib/init.verb > test-output/init-c-BOOT.txt
python compiler.py ../lib/compiler.verb > test-output/compiler-c-BOOT.txt

echo "C++ ..."
# check with c++ port running compiler.verb
../c++/repl ../lib/compiler.verb -- ../lib/init.verb > test-output/init-c-CPP.txt
../c++/repl ../lib/compiler.verb -- ../lib/compiler.verb > test-output/compiler-c-CPP.txt

diff -u -b -B test-output/init-c-CPP.txt test-output/init-c-BOOT.txt
diff -u -b -B test-output/compiler-c-CPP.txt test-output/compiler-c-BOOT.txt

echo "Python ..."
# check with python port running compiler.verb
python ../new-python/repl.py ../lib/compiler.verb -- ../lib/init.verb > test-output/init-c-PY.txt
python ../new-python/repl.py ../lib/compiler.verb -- ../lib/compiler.verb > test-output/compiler-c-PY.txt

diff -u -b -B test-output/init-c-PY.txt test-output/init-c-BOOT.txt
diff -u -b -B test-output/compiler-c-PY.txt test-output/compiler-c-BOOT.txt

echo "Lua ..."
# check with lua port running compiler.verb
( cd ../lua ; lua repl.lua ../lib/compiler.verb -- ../lib/init.verb > ../bootstrap/test-output/init-c-LUA.txt )
( cd ../lua ; lua repl.lua ../lib/compiler.verb -- ../lib/compiler.verb > ../bootstrap/test-output/compiler-c-LUA.txt )

diff -u -b -B test-output/init-c-LUA.txt test-output/init-c-BOOT.txt
diff -u -b -B test-output/compiler-c-LUA.txt test-output/compiler-c-BOOT.txt

echo "C# ..."
# check with c# port
../csharp/repl.exe ../lib/compiler.verb -- ../lib/init.verb > test-output/init-c-CSHARP.txt
../csharp/repl.exe ../lib/compiler.verb -- ../lib/compiler.verb > test-output/compiler-c-CSHARP.txt

diff -u -b -B test-output/init-c-CSHARP.txt test-output/init-c-BOOT.txt
diff -u -b -B test-output/compiler-c-CSHARP.txt test-output/compiler-c-BOOT.txt


