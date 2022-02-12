#!/bin/bash

# perform a diff of expected output from unittest vs actual output, for all ports.
# puts results in RESULTS/PORT/diff-TEST.txt
#
# run the regression.sh/regression.bat scripts in each port before running this.

declare -a Ports=("c++" "csharp" "lua" "python" )
declare -a Tests=("core" "basic" "errors" )

for lang in ${Ports[@]}; 
do
	for testname in ${Tests[@]};
	do
		expectdir="."
		actualdir="./RESULTS/$lang"
		expect="expect_unittest_$testname.txt"
		actual="actual_unittest_$testname.txt"
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

