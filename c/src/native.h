/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#ifndef __native_h__
#define __native_h__

#include "interpreter.h"
#include "langtypes.h"

//extern Object *BUILTINS;

void init_builtins();

// returns index (which can be passed to call_builtin_by_index()) or
// -1 if no such builtin
int lookup_builtin_index(const char *name);
void call_builtin_by_index(int i);
int get_number_of_builtins();

// set from main()
extern Object *native_cmdline_args;

extern int ALLOW_OVERWRITING_WORDS;

extern int EXIT_ON_EXCEPTION;

extern int STACKTRACE_ON_EXCEPTION;

// cpu-time reports the amount of time since verbii started, so
// this is set immediately upon startup
extern double STARTUP_TIME;

// for gc-object 
void native_mark_reachable_objects();

#endif // __native_h__



