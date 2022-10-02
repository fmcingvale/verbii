/*
	xmalloc - wrapper to allow turning GC on/off completely for debugging, etc.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once

#if defined(USE_GCMALLOC)
#include <gc_cpp.h>
#else // !USE_GCMALLOC
#include <stdlib.h>
extern unsigned long long X_BYTES_ALLOCATED;
#endif // USE_GCMALLOC

void x_mem_init();
void* x_malloc(size_t size);
void* x_realloc(void *ptr, size_t new_size);
void x_mem_gcollect();
char *x_strndup(const char *s, size_t n);
void x_free(void *ptr);
