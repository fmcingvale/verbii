/*
	xmalloc - wrapper to allow turning GC on/off completely for debugging, etc.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#ifndef __xmalloc_h__
#define __xmalloc_h__

#if defined(USE_BOEHM_GC)
#include <gc.h>
#else // !USE_BOEHM_GC
#include <stdlib.h>
extern unsigned long long X_BYTES_ALLOCATED;
#if defined(USE_GC_OBJECT)
extern unsigned long long X_BYTES_FREED;
#endif // USE_GC_OBJECT
#endif // !USE_BOEHM_GC

void x_mem_init();
void* x_malloc(size_t size);
void* x_realloc(void *ptr, size_t new_size);
void x_mem_gcollect();
char *x_strdup(const char *s);
char *x_strndup(const char *s, size_t n);
void x_free(void *ptr);

#endif // __xmalloc_h__
