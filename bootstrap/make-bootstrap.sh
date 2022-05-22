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
	declare -a Libraries=("init" "compiler")	
	for name in ${Libraries[@]}; do
		echo "Bootstrapping " "$name.verb"
		
		# causes an error to overwrite (at least) compiler.verb.b directly,
		# so save to temp file then copy
		$REPL ../verb/compile.verb -- ../lib/$name.verb > ../lib/$name.verb.b.temp
		cp ../lib/$name.verb.b.temp ../lib/$name.verb.b
		rm ../lib/$name.verb.b.temp
	done
else
	echo ""
	echo "Exiting"
fi

