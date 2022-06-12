#!/bin/bash

# clear all cache files and leftovers from unittests

# remove all .b files ... but NOT in lib/
rm verb/*.verb.b 
rm unittests/*.verb.b

# remove cache files from unittests
rm `find -name "*.verb.lastline"`
