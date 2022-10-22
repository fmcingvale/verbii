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

char *the_real_x_strndup(const char *filename, int linenr, const char *s, size_t n) {
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
	#if defined(USE_XMEM_TRACE)
	struct _MemBlockHeader *prev, *next;
	const char *filename;
	int linenr;
	#endif
} MemBlockHeader;
#endif // USE_GC_OBJECT

unsigned long long XMEM_USER_BYTES_ALLOCATED = 0;
unsigned long long XMEM_TOTAL_BYTES_ALLOCATED = 0;
unsigned long long XMEM_USER_BYTES_FREED = 0; // only GC_OBJECT uses this
unsigned long long XMEM_TOTAL_BYTES_FREED = 0; // only GC_OBJECT uses this
unsigned long long XMEM_MAX_MEMORY_SIZE = 0; // only GC_OBJECT uses this
unsigned long long XMEM_BYTES_SINCE_GC = 0; // only GC_OBJECT uses this

#if defined(USE_XMEM_TRACE)
static MemBlockHeader *XMEM_HEAD = NULL;
static MemBlockHeader *XMEM_TAIL = NULL;
#endif
void x_mem_init() {
#if defined(USE_XMEM_TRACE)
	//printf("TRACE INIT\n");
	XMEM_HEAD = (MemBlockHeader*)malloc(sizeof(MemBlockHeader));
	XMEM_HEAD->next = NULL;
	XMEM_HEAD->prev = NULL;
	XMEM_TAIL = XMEM_HEAD;
#endif
}

#if defined(USE_XMEM_TRACE)
static void add_xmem_node(MemBlockHeader *node, const char *filename, int linenr) {
	XMEM_TAIL->next = node;
	node->next = NULL;
	node->prev = XMEM_TAIL;
	node->filename = filename;
	node->linenr = linenr;
	XMEM_TAIL = node;
}

static void remove_xmem_node(MemBlockHeader *node) {
	node->prev->next = node->next;
	if(node->next)
		node->next->prev = node->prev;

	if(node == XMEM_TAIL)
		XMEM_TAIL = node->prev;
}
#endif

void* the_real_x_malloc(const char *filename, int line, size_t size) {
	//printf("alloc @ %s:%d size=%d\n", filename, line, (int)size);
#if defined(USE_GC_OBJECT)
	void *ptr = malloc(size + sizeof(MemBlockHeader));
	MemBlockHeader *head = (MemBlockHeader*)ptr;
	head->magic = MEMBLOCK_MAGIC;
	head->size = size;
	XMEM_USER_BYTES_ALLOCATED += size;
	XMEM_TOTAL_BYTES_ALLOCATED += size + sizeof(MemBlockHeader);
	XMEM_BYTES_SINCE_GC += size;
	// track max memory allocated at any time
	XMEM_MAX_MEMORY_SIZE = max(XMEM_MAX_MEMORY_SIZE, XMEM_TOTAL_BYTES_ALLOCATED - XMEM_TOTAL_BYTES_FREED);
	#if defined(USE_XMEM_TRACE)
	// add to linked list
	add_xmem_node(head, filename, line);
	#endif // USE_XMEM_TRACE
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

	#if defined(USE_XMEM_TRACE)
	// remove from linked list before reallocating
	remove_xmem_node(head);
	#endif
	// i don't want this to count as a full free+malloc, so instead, subtract
	// current size then i'll add new size
	XMEM_USER_BYTES_ALLOCATED -= head->size;
	XMEM_TOTAL_BYTES_ALLOCATED -= head->size; // the overhead is constant so just change the size portion
	XMEM_BYTES_SINCE_GC -= head->size; // not sure if subtracting is the best thing for this stat; maybe I should only add?
	void *newptr = realloc((void*)head, new_size + sizeof(MemBlockHeader));
	if(!newptr)
		error("Out of memory in realloc()");

	head = (MemBlockHeader*)newptr;
	head->size = new_size;
	XMEM_USER_BYTES_ALLOCATED += new_size;
	XMEM_TOTAL_BYTES_ALLOCATED += new_size; // as above, overhead already accounted for
	XMEM_BYTES_SINCE_GC += new_size;
	#if defined(USE_XMEM_TRACE)
	add_xmem_node(head, filename, line);
	#endif // XMEM_TRACE
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

char *the_real_x_strndup(const char *filename, int linenr, const char *s, size_t n) {
	n = min(strlen(s),n);
	
	char *buf = (char*)the_real_x_malloc(filename, linenr, n+1);
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
	#if defined(USE_XMEM_TRACE)
	remove_xmem_node(head);
	#endif
	free(head);
#else // no-GC case
	free(ptr);
#endif
}

void x_mem_print_stats() {
#if defined(USE_GC_OBJECT)
	printf("  Garbage collector: gc-object\n");
	printf("  max memory in use:             %llu\n", XMEM_MAX_MEMORY_SIZE);
	printf("  xmalloc user bytes allocated:  %llu\n", XMEM_USER_BYTES_ALLOCATED);
	printf("  xmalloc user bytes freed:      %llu\n", XMEM_USER_BYTES_FREED);
	printf("  xmalloc total bytes allocated: %llu\n", XMEM_TOTAL_BYTES_ALLOCATED);
	printf("  xmalloc total bytes freed:     %llu\n", XMEM_TOTAL_BYTES_FREED);
#else // no-GC
	printf("  Garbage collector: None\n");	
	printf("  xmalloc bytes: %llu\n", XMEM_TOTAL_BYTES_ALLOCATED);
#endif
}

void x_mem_print_trace() {
#if defined(USE_XMEM_TRACE) && defined(USE_GC_OBJECT)
	printf("  Memory trace:\n");
	MemBlockHeader *node = XMEM_HEAD->next; // first node is fake
	while(node) {
		printf("%s @ %d, %ld bytes\n", node->filename, node->linenr, node->size);
		node = node->next;
	}
#endif
}

#endif // USE_BOEHM_GC

#if 0
char *x_strdup(const char *s) {
	return x_strndup(s, strlen(s));
}
#endif


