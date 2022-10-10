/*
	xmalloc - wrapper to allow turning GC on/off completely for debugging, etc.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "xmalloc.h"
#include "langtypes.h"
#include "errors.h"
#include <string.h>

#if defined(USE_BOEHM_GC)

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

void x_free(void *ptr) { }

#else // !USE_BOEHM_GC

// for USE_GC_OBJECT, I add a header to each allocated block so I can tell how many
// bytes get freed in x_free(). For the non-gc case, I turn this off because I want
// the non-GC case to be pure vanilla malloc.
#if defined(USE_GC_OBJECT)
#define MEMBLOCK_MAGIC 0xe8ab317f

typedef struct _MemBlockHeader {
	size_t size; // size of memory block, NOT counting this header
	int magic; // MEMBLOCK_MAGIC
} MemBlockHeader;
#endif // USE_GC_OBJECT

unsigned long long X_BYTES_ALLOCATED = 0;
unsigned long long X_BYTES_FREED = 0; // only GC_OBJECT uses this

void x_mem_init() {
}

void* x_malloc(size_t size) {
#if defined(USE_GC_OBJECT)
	void *ptr = malloc(size + sizeof(MemBlockHeader));
	MemBlockHeader *head = (MemBlockHeader*)ptr;
	head->magic = MEMBLOCK_MAGIC;
	head->size = size;
	X_BYTES_ALLOCATED += size;
	return (void*)(((char*)ptr) + sizeof(MemBlockHeader));
#else // no-GC case
	X_BYTES_ALLOCATED += size;
	return malloc(size);
#endif
}

void* x_realloc(void *ptr, size_t new_size) {
#if defined(USE_GC_OBJECT)
	// special case
	if(!ptr)
		return x_malloc(new_size);

	MemBlockHeader *head = (MemBlockHeader*)(((char*)ptr) - sizeof(MemBlockHeader));
	if(head->magic != MEMBLOCK_MAGIC)
		error("Bad magic number in x_realloc()");

	// i don't want this to count as a full free+malloc, so instead, subtract
	// current size then i'll add new size
	X_BYTES_ALLOCATED -= head->size;
	void *newptr = realloc((void*)head, new_size + sizeof(MemBlockHeader));
	if(!newptr)
		error("Out of memory in realloc()");

	head = (MemBlockHeader*)newptr;
	head->size = new_size;
	X_BYTES_ALLOCATED += new_size;
	return ((char*)newptr) + sizeof(MemBlockHeader);
#else // non-GC case
	return realloc(ptr, new_size);
#endif
}

// for USE_GC_OBJECT, garbage collection is handled at the object level, not here.
// for no-GC, then nothing to do (of course, memory will grow unbounded, but non-GC
// is only really meant for debugging)
void x_mem_gcollect() {
}

char *x_strndup(const char *s, size_t n) {
	n = min(strlen(s),n);
	
	char *buf = (char*)x_malloc(n+1);
	memcpy(buf, s, n);
	buf[n] = 0;
	return buf;
}

void x_free(void *ptr) {
#if defined(USE_GC_OBJECT)
	MemBlockHeader *head = (MemBlockHeader*)(((char*)ptr) - sizeof(MemBlockHeader));
	if(head->magic != MEMBLOCK_MAGIC)
		error("Bad magic number in x_realloc()");

	X_BYTES_FREED += head->size;
	free(head);
#else // no-GC case
	free(ptr);
#endif
}

#endif // USE_BOEHM_GC

char *x_strdup(const char *s) {
	return x_strndup(s, strlen(s));
}

