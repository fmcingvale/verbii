/*
	Error handling

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "errors.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

void error(const char *fmt, ...) {
	char msg[512];
	va_list args;
	va_start(args,fmt);
	vsnprintf(msg, sizeof(msg), fmt, args);
	va_end(args);
	printf("%s\n", msg);
	// do a longjmp or something else later
	exit(1);
}
