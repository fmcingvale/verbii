#!/bin/sh
#
# Setup build for linux
rm -rf bin/
cmake -DCMAKE_BUILD_TYPE=Release -S src/ -B bin

