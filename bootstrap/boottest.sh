#!/bin/bash
#
# test that bootstrap compiler produces identical output to compiler.verb,
# on init.verb and compiler.verb
#
# should run this as a sanity check before running make-bootstrap.sh

echo "There should be no output below this line:"

mkdir -p test-output

python compiler.py ../lib/init.verb > test-output/init-c-PY.txt
python compiler.py ../lib/compiler.verb > test-output/compiler-c-PY.txt

# currently only c++ version can run compiler.verb
../c++/repl ../lib/compiler.verb -- ../lib/init.verb > test-output/init-c-VERB.txt
../c++/repl ../lib/compiler.verb -- ../lib/compiler.verb > test-output/compiler-c-VERB.txt

diff -u -b -B test-output/init-c-VERB.txt test-output/init-c-PY.txt
diff -u -b -B test-output/compiler-c-VERB.txt test-output/compiler-c-PY.txt
