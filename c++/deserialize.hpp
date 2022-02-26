/*
	Deserialize - load bytecode produced by compiler.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once
#include <iostream>
#include <fstream>
#include <string>
#include "langtypes.hpp"
#include "interpreter.hpp"

// deserialize byte-compiled stream into interpreter
Object deserialize_stream(Interpreter *intr, std::ifstream &fileIn);
