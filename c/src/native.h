/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#ifndef __native_h__
#define __native_h__

#include "interpreter.h"
#include "langtypes.h"

extern Object *BUILTINS;

void init_builtins();

// set from main()
extern Object *native_cmdline_args;

extern int ALLOW_OVERWRITING_WORDS;

extern int EXIT_ON_EXCEPTION;

extern int STACKTRACE_ON_EXCEPTION;

// cpu-time reports the amount of time since verbii started, so
// this is set immediately upon startup
extern double STARTUP_TIME;

#endif // __native_h__



