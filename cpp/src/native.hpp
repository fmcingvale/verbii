/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#pragma once
#include <map>
#include <string>
#include "interpreter.hpp"
#include "langtypes.hpp"

typedef void (*BUILTIN_FUNC)(Interpreter *);

extern std::map<std::string,BUILTIN_FUNC> BUILTINS;

// set from main()
extern Object native_cmdline_args;

extern bool ALLOW_OVERWRITING_WORDS;

extern bool EXIT_ON_EXCEPTION;

extern bool STACKTRACE_ON_EXCEPTION;

// cpu-time reports the amount of time since verbii started, so
// this is set immediately upon startup
extern double STARTUP_TIME;




