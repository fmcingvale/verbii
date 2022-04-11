#!/bin/bash

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
( cd new-python; ./regression.sh )

# chicken
echo "** Chicken ..."
( cd chicken; make clean; make; ./regression.sh )
