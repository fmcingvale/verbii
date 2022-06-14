#!/bin/sh

echo "Build verbii.exe ..."
csc -debug -out:verbii main.cs interpreter.cs langtypes.cs errors.cs native.cs deserialize.cs
