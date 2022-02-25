/*
	Deserialize - load bytecode produced by compiler.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once
#include <iostream>
#include <fstream>
#include <string>
#include "langtypes.hpp"

Object deserialize_stream(std::ifstream &fileIn);
