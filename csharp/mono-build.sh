#!/bin/sh

echo "Build main.exe ..."
csc -debug -out:main.exe main.cs interpreter.cs langtypes.cs errors.cs native.cs deserialize.cs opcodes.cs
