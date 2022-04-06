#!/bin/bash

echo "********************************************************"
echo " Make sure to run cleaning script and/or 'make clean'"
echo " in each port folder before running this."
echo "********************************************************"

# count lines of code for all ports
#
# note that .verb files are most similar to Forth, so using that to count
cloc --include-ext=cpp,hpp,cs,py,lua,verb,scm --force-lang="Forth",verb --force-lang="C++",hpp --exclude-dir=alphaexperiments,ATTIC,python,bootstrap .