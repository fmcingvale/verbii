REM 
REM For running regression tests under pypy on Windows
REM

pypy repl.py -test ../unittests/unittest_core.verb > ../unittests/RESULTS/python/actual_unittest_core.txt
pypy repl.py -test ../unittests/unittest_basic.verb > ../unittests/RESULTS/python/actual_unittest_basic.txt
pypy repl.py -test ../unittests/unittest_errors.verb > ../unittests/RESULTS/python/actual_unittest_errors.txt

REM demos are full programs, so run without -test
pypy repl.py ../unittests/unittest_demo.verb > ../unittests/RESULTS/python/actual_unittest_demo.txt

pypy repl.py ../unittests/demo_math.verb > ../unittests/RESULTS/python/actual_demo_math.txt
