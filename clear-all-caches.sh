#!/bin/bash

# clear all cache files and leftovers from unittests

# remove all .b files ... but NOT in lib/
rm -f verb/*.verb.b 
rm -f unittests/*.verb.b

# remove cache files from unittests
rm -f `find -name "*.verb.lastline"`
