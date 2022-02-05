
// wrapper to allow turning GC on/off completely for debugging, etc.
#pragma once

#if defined(USE_GCMALLOC)
#include <gc/gc_cpp.h>
#else // !USE_GCMALLOC
#include <stdlib.h>
#endif // USE_GCMALLOC

void x_mem_init();
void* x_malloc(size_t size);
void x_mem_gcollect();
