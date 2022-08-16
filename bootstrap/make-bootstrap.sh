#!/bin/bash

PRODUCTION=1
# define which verbii port is considered the production compiler (all should produce identical output,
# so it's really just a matter of which one is easiest to run on your system)
#
# i usually implement new things in the c++ compiler first, so that is a primary reason why
# the c++ port is the default. also it is the fastest implementation.
VERBII='../cpp/verbii'

# set VERBII_BOOT since this should always run from this location.
# this is convenient since it doesn't require it to be preset as well
# as allowing the C# tests to run under WSL which for some reason
# cannot handle absolute paths from /mnt ...
export VERBII_BOOT=../lib/

# a sanity check in case this was run inadvertently ...
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
	echo "Building with $VERBII"
	echo ""
	declare -a Libraries=("boot" "init" "compiler")	
	# do this in two steps:
	#    (1) Compile all library files to temporary files
	#    (2) After all are compiled, copy temp files over .b files
	#
	# this is needed especially in cases where there are interdependent changes
	# between the files -- e.g. a function is moved from compiler.verb to init.verb --
	# if the init.verb.b was replaced immediately, the compiler would then fail due to seeing
	# a duplicate name between the new init.verb.b and the old compiler.verb.b file. 
	# another example would be if a function name changed -- the old .b would still expect 
	# the old name to exist and fail. this way is safer and keeps the .b files synced.
	for name in ${Libraries[@]}; do
		echo "Compiling " "$name.verb"

		# use -nocache to be safer in case i'm compiling changes to the caching code
		# (PROBABLY this was only needed when the caching code was first added via patches.verb,
		# to get around the chicken-and-egg problem of being unable to load patches.verb.b,
		# but doesn't hurt to leave it in)
		$VERBII ../verb/compile.verb -nocache -- ../lib/$name.verb > ../lib/$name.verb.b.temp
	done
	# *after* all are compiled, copy the temp files over the .b files
	for name in ${Libraries[@]}; do
		echo "Installing " "$name.verb.b"
		cp ../lib/$name.verb.b.temp ../lib/$name.verb.b
		rm ../lib/$name.verb.b.temp
	done
else
	echo ""
	echo "Exiting"
fi

