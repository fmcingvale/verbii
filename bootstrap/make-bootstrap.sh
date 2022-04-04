#!/bin/bash

PRODUCTION=0

if [ $PRODUCTION -eq 1 ]; then
	echo "* CAUTION *"
	echo ""
	echo "You are about to overwrite your bootstrapped library."
	echo ""
	echo "Make sure ./boottest.sh passes (runs with no output) before running this."
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
		#python compiler.py ../lib/$name.verb > ../lib/$name.verb.b
		
		# causes an error to overwrite (at least) compiler.verb.b directly,
		# so save to temp file then copy
		../c++/repl ../verb/compile.verb -- ../lib/$name.verb > ../lib/$name.verb.b.temp
		cp ../lib/$name.verb.b.temp ../lib/$name.verb.b
		rm ../lib/$name.verb.b.temp
	done
else
	echo ""
	echo "Exiting"
fi

