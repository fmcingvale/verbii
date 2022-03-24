#!/bin/bash

# count lines of code for all ports
#
# note that .verb files are most similar to Forth, so using that to count
cloc --include-ext=cpp,hpp,cs,py,lua,verb,scm --force-lang="Forth",verb --force-lang="C++",hpp --exclude-dir=alphaexperiments,ATTIC,python .