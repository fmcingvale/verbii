/*
	Error handling

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#ifndef __errors_h__
#define __errors_h__

#include <setjmp.h>

_Noreturn void error(const char *fmt, ...);

extern jmp_buf ERROR_JMP_BUF;
// read-only; only for use from sethmp caller
extern char *ERROR_MESSAGE;

#endif
