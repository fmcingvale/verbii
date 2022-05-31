#!/bin/bash

PRODUCTION=1
# define which repl is considered the production compiler (all should produce identical output,
# so it's really just a matter of which one is easiest to run)
#
# i usually implement new things in the c++ compiler first, so that is a primary reason why
# the c++ repl is the default
REPL='../c++/repl'

if [ $PRODUCTION -eq 1 ]; then
	echo "                     *** CAUTION ***"
	echo ""
	echo "** You are about to overwrite your bootstrapped compiler & library."
	echo ""
	echo "** Doing this without sufficient testing can break the entire system!"
	echo ""
	echo "** Make sure you have read \"READ-ME-BEFORE-RUNNING.txt\" before running this!"
	echo ""
	echo -n "Type YES if you really want to do this, anything else to exit: "
	read line
else
	line="YES"
fi

if [ "$line" == "YES" ]; 
then
	echo ""
	declare -a Libraries=("boot" "init" "compiler")	
	for name in ${Libraries[@]}; do
		echo "Bootstrapping " "$name.verb"
		
		# trying to overwrite (at least) compiler.verb.b directly causes an error,
		# so save to temp file then copy
		#
		# use -nocache to be safer in case i'm compiling changes to the caching code
		# (PROBABLY this was only needed when the caching code was first added via patches.verb,
		# to get around the chicken-and-egg problem of being unable to load patches.verb.b,
		# but doesn't hurt to leave it in)
		$REPL ../verb/compile.verb -nocache -- ../lib/$name.verb > ../lib/$name.verb.b.temp
		cp ../lib/$name.verb.b.temp ../lib/$name.verb.b
		rm ../lib/$name.verb.b.temp
	done
else
	echo ""
	echo "Exiting"
fi

