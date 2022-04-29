#!/bin/bash

# clean out ALL ports - useful before running count-code.sh, for example

# c++
( cd c++; make clean )

# c#
( cd csharp; ./mono-clean.sh )

# python
( cd python; rm -rf __pycache__ )

# chicken
( cd chicken; make clean )

# currently nothing to clean for lua
