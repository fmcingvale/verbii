/*
	xmalloc - wrapper to allow multiple memory managers

	Currently supports: Boehm GC, custom Object-GC and no GC (mainly for debugging)

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "xmalloc.h"
#include "langtypes.h"
#include "errors.h"
#include <string.h>
#include <stdio.h>

#if defined(USE_BOEHM_GC)

void x_mem_init() {
	GC_INIT();
}

void* the_real_x_malloc(const char *filename, int line, size_t size) {
	return GC_malloc(size);
}

void* the_real_x_realloc(const char *filename, int line, void *ptr, size_t new_size) {
	return GC_realloc(ptr, new_size);
}

void x_mem_gcollect() {
	GC_gcollect();
}

char *x_strndup(const char *s, size_t n) {
	return GC_strndup(s, n);
}

void x_free(void *ptr) { }

void x_mem_print_stats() {
	printf("  Garbage collector: Boehm-GC\n");
	GC_word pheap_size, pfree_bytes, punmapped_bytes, pbytes_since_gc, ptotal_bytes;
	GC_get_heap_usage_safe(&pheap_size, &pfree_bytes, &punmapped_bytes, &pbytes_since_gc, &ptotal_bytes);
	printf("  Heap size: %lu\n", pheap_size);
	printf("  Free bytes: %lu\n", pfree_bytes);
	printf("  Unmapped bytes: %lu\n", punmapped_bytes);
	printf("  Bytes since gc: %lu\n", pbytes_since_gc);
	printf("  Total bytes: %lu\n", ptotal_bytes);
}

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

unsigned long long XMEM_USER_BYTES_ALLOCATED = 0;
unsigned long long XMEM_TOTAL_BYTES_ALLOCATED = 0;
unsigned long long XMEM_USER_BYTES_FREED = 0; // only GC_OBJECT uses this
unsigned long long XMEM_TOTAL_BYTES_FREED = 0; // only GC_OBJECT uses this

void x_mem_init() {
}

void* the_real_x_malloc(const char *filename, int line, size_t size) {
	//printf("alloc @ %s:%d size=%d\n", filename, line, (int)size);
#if defined(USE_GC_OBJECT)
	void *ptr = malloc(size + sizeof(MemBlockHeader));
	MemBlockHeader *head = (MemBlockHeader*)ptr;
	head->magic = MEMBLOCK_MAGIC;
	head->size = size;
	XMEM_USER_BYTES_ALLOCATED += size;
	XMEM_TOTAL_BYTES_ALLOCATED += size + sizeof(MemBlockHeader);
	return (void*)(((char*)ptr) + sizeof(MemBlockHeader));
#else // no-GC case
	XMEM_USER_BYTES_ALLOCATED += size;
	XMEM_TOTAL_BYTES_ALLOCATED += size; // no extra overhead for the no-GC case
	return malloc(size);
#endif
}

void* the_real_x_realloc(const char *filename, int line, void *ptr, size_t new_size) {
#if defined(USE_GC_OBJECT)
	// special case
	if(!ptr)
		return the_real_x_malloc(filename,line,new_size);

	MemBlockHeader *head = (MemBlockHeader*)(((char*)ptr) - sizeof(MemBlockHeader));
	if(head->magic != MEMBLOCK_MAGIC)
		error("Bad magic number in x_realloc()");

	// i don't want this to count as a full free+malloc, so instead, subtract
	// current size then i'll add new size
	XMEM_USER_BYTES_ALLOCATED -= head->size;
	XMEM_TOTAL_BYTES_ALLOCATED -= head->size; // the overhead is constant so just change the size portion
	void *newptr = realloc((void*)head, new_size + sizeof(MemBlockHeader));
	if(!newptr)
		error("Out of memory in realloc()");

	head = (MemBlockHeader*)newptr;
	head->size = new_size;
	XMEM_USER_BYTES_ALLOCATED += new_size;
	XMEM_TOTAL_BYTES_ALLOCATED += new_size; // as above, overhead already accounted for
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

	XMEM_USER_BYTES_FREED += head->size;
	XMEM_TOTAL_BYTES_FREED += head->size + sizeof(MemBlockHeader);
	free(head);
#else // no-GC case
	free(ptr);
#endif
}

void x_mem_print_stats() {
#if defined(USE_GC_OBJECT)
	printf("  Garbage collector: gc-object\n");
	printf("  xmalloc user bytes allocated:  %llu\n", XMEM_USER_BYTES_ALLOCATED);
	printf("  xmalloc user bytes freed:      %llu\n", XMEM_USER_BYTES_FREED);
	printf("  xmalloc total bytes allocated: %llu\n", XMEM_TOTAL_BYTES_ALLOCATED);
	printf("  xmalloc total bytes freed:     %llu\n", XMEM_TOTAL_BYTES_FREED);
#else // no-GC
	printf("  Garbage collector: None\n");	
	printf("  xmalloc bytes: %llu\n", XMEM_TOTAL_BYTES_ALLOCATED);
#endif
}

#endif // USE_BOEHM_GC

char *x_strdup(const char *s) {
	return x_strndup(s, strlen(s));
}

