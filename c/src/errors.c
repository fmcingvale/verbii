/*
	Error handling

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "errors.h"
#include "xmalloc.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <setjmp.h>

jmp_buf ERROR_JMP_BUF;
char *ERROR_MESSAGE = NULL;

void error(const char *fmt, ...) {
	char temp[2];
	va_list args;
	va_start(args,fmt);
	int nr = vsnprintf(temp, 1, fmt, args);
	va_end(args);

	if(ERROR_MESSAGE)
		x_free(ERROR_MESSAGE);

	ERROR_MESSAGE = (char*)x_malloc(nr+2);
	va_start(args,fmt);
	vsnprintf(ERROR_MESSAGE, nr+1, fmt, args);
	va_end(args);

	longjmp(ERROR_JMP_BUF,1);
}
