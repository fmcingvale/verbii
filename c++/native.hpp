/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#pragma once
#include <map>
#include <string>
#include "interpreter.hpp"

typedef void (*BUILTIN_FUNC)(Interpreter *);

extern std::map<std::string,BUILTIN_FUNC> BUILTINS;





