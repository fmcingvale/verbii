/*
	Deserialize - load bytecode produced by compiler.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#ifndef __deserialize_h__
#define __deserialize_h__

#include "langtypes.h"
#include "interpreter.h"
#include <stdio.h>

// deserialize byte-compiled file into interpreter
Object* deserialize_stream(FILE *fp);

#endif // __deserialize_h__