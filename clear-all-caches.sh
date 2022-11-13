#!/bin/bash

# clear all cache files and leftovers from unittests

# remove all .b files ... but NOT in lib/
rm -f verb/*.verb.b 
rm -f verb/chess/*.verb.b
rm -f unittests/*.verb.b

# selectively remove files from lib/ -- NEVER delete boot.b, init.b nor compiler.b here
declare -a Libfiles=("hashing" "numeric" "paths" "random-mersenne" "random-xorshift32" "random-xoshiro128++" 
					"random-xoshiro128starstar" "repl" "sorting" "unittest" )

for name in ${Libfiles[@]}; 
do
	rm -f lib/$name.verb.b 
done

# remove cache files from unittests
rm -f `find -name "*.verb.lastline"`
