#!/bin/sh
#
# Setup build for linux
rm -rf bin/
cmake -DCMAKE_BUILD_TYPE=Debug -S src/ -B bin

