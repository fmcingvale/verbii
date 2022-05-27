#!/bin/bash

# clean out previous results to ensure everything is being run below
find unittests/RESULTS/ -type f -print0 | xargs -0 rm -vf

# kind of redundant but gets things otherwise missed .. need to refactor these scripts
./clean-all-ports.sh

# run ALL regression tests -- make fresh executables where applicable

# c++
echo "** C++ ..."
( cd c++; make clean; make; ./regression.sh )

# c#
echo "** C# ..."
( cd csharp; ./mono-clean.sh; ./mono-build.sh; ./mono-regression.sh )

# lua
echo "** Lua ..."
( cd lua; ./regression.sh )

# python
echo "** Python ..."
( cd python; ./regression.sh )

# chicken
echo "** Chicken ..."
( cd chicken; make clean; make; ./regression.sh )
