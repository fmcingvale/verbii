#!/bin/bash

# clean out ALL ports - useful before running count-code.sh, for example

# c++
( cd cpp; ./clean.sh )

# c#
( cd csharp; ./mono-clean.sh )

# python
( cd python; rm -rf __pycache__ )

# chicken
( cd chicken; make clean )

# currently nothing to clean for lua

# remove all .b files and other caches
./clear-all-caches.sh



