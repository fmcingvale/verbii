#!/bin/sh

echo "Build repl.exe ..."
csc -debug repl.cs interpreter.cs langtypes.cs errors.cs native.cs deserialize.cs

echo "Build min.exe ..."
csc -debug min.cs interpreter.cs langtypes.cs errors.cs native.cs deserialize.cs