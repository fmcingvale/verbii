#!/bin/bash

# perform a diff of expected output from unittest vs actual output, for all ports.
# puts results in RESULTS/PORT/diff-*.txt
#
# run the regression.sh/regression.bat scripts in each port before running this.

declare -a Ports=("cpp" "csharp" "lua" "python" "chicken" )
declare -a Tests=("unittest_core" "unittest_basic" "unittest_errors" "unittest_demo" "demo_math" )

for lang in ${Ports[@]}; 
do
	for testname in ${Tests[@]};
	do
		expectdir="."
		actualdir="./RESULTS/$lang"
		expect="expect_$testname.txt"
		actual="actual_$testname.txt"
		diffname="diff_$testname.txt"
		cmd="diff -u -b -B $expectdir/$expect $actualdir/$actual > $actualdir/$diffname"
		echo $cmd
		diff -u -b -B $expectdir/$expect $actualdir/$actual > $actualdir/$diffname
	done
done

for lang in ${Ports[@]}; 
do
	wc -l ./RESULTS/$lang/diff*
done

