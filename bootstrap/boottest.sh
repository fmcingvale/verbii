#!/bin/bash
#
# test that bootstrap compiler produces identical output to compiler.verb,
# on init.verb and compiler.verb
#
# should run this as a sanity check before running make-bootstrap.sh

echo "There should be no output below this line:"

mkdir -p test-output
rm test-output/*

# use python bootstrap compiler first
python compiler.py ../lib/init.verb > test-output/init-c-BOOT.txt
python compiler.py ../lib/compiler.verb > test-output/compiler-c-BOOT.txt

# check with c++ port running compiler.verb
../c++/repl ../lib/compiler.verb -- ../lib/init.verb > test-output/init-c-CPP.txt
../c++/repl ../lib/compiler.verb -- ../lib/compiler.verb > test-output/compiler-c-CPP.txt

diff -u -b -B test-output/init-c-CPP.txt test-output/init-c-BOOT.txt
diff -u -b -B test-output/compiler-c-CPP.txt test-output/compiler-c-BOOT.txt

# check with python port running compiler.verb
python ../new-python/repl.py ../lib/compiler.verb -- ../lib/init.verb > test-output/init-c-PY.txt
python ../new-python/repl.py ../lib/compiler.verb -- ../lib/compiler.verb > test-output/compiler-c-PY.txt

diff -u -b -B test-output/init-c-PY.txt test-output/init-c-BOOT.txt
diff -u -b -B test-output/compiler-c-PY.txt test-output/compiler-c-BOOT.txt
