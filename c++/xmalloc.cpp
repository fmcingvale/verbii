/*
	xmalloc - wrapper to allow turning GC on/off completely for debugging, etc.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "xmalloc.hpp"

#if defined(USE_GCMALLOC)

void x_mem_init() {
	GC_INIT();
}

void* x_malloc(size_t size) {
	return GC_malloc(size);
}

void* x_realloc(void *ptr, size_t new_size) {
	return GC_realloc(ptr, new_size);
}

void x_mem_gcollect() {
	GC_gcollect();
}

char *x_strndup(const char *s, size_t n) {
	return GC_strndup(s, n);
}

#else // !USE_GCMALLOC
#include <string.h>

unsigned long long X_BYTES_ALLOCATED = 0;

void x_mem_init() {
}

void* x_malloc(size_t size) {
	X_BYTES_ALLOCATED += size;
	return malloc(size);
}

void* x_realloc(void *ptr, size_t new_size) {
	return realloc(ptr, new_size);
}

void x_mem_gcollect() {
}

char *x_strndup(const char *s, size_t n) {
	X_BYTES_ALLOCATED += n + 1;
	return strndup(s, n);
}

#endif // USE_GCMALLOC

