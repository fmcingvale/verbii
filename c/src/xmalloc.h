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
// bytes given to the user (does not count internal overhead)
extern unsigned long long XMEM_USER_BYTES_ALLOCATED;
// total bytes including overhead
extern unsigned long long XMEM_TOTAL_BYTES_ALLOCATED;
#if defined(USE_GC_OBJECT)
extern unsigned long long XMEM_USER_BYTES_FREED;
extern unsigned long long XMEM_TOTAL_BYTES_FREED;
extern unsigned long long XMEM_MAX_MEMORY_SIZE;
extern unsigned long long XMEM_BYTES_SINCE_GC;
#endif // USE_GC_OBJECT
#endif // !USE_BOEHM_GC

void x_mem_init();
void* the_real_x_malloc(const char *filename, int line, size_t size);
#define x_malloc(size) the_real_x_malloc(__FILE__,__LINE__,size)
void* the_real_x_realloc(const char *filename, int line, void *ptr, size_t new_size);
#define x_realloc(ptr,size) the_real_x_realloc(__FILE__,__LINE__,ptr,size)
void x_mem_gcollect();
//char *x_strdup(const char *s);
char *the_real_x_strndup(const char *filename, int linenr, const char *s, size_t n);
#define x_strndup(s,n) the_real_x_strndup(__FILE__,__LINE__,s,n)
#define x_strdup(s) the_real_x_strndup(__FILE__,__LINE__,s,strlen(s))
void x_free(void *ptr);

void x_mem_print_stats();
void x_mem_print_trace();

#endif // __xmalloc_h__
