
#include "xmalloc.hpp"

#if defined(USE_GCMALLOC)

void x_mem_init() {
	GC_INIT();
}

void* x_malloc(size_t size) {
	return GC_malloc(size);
}

void x_mem_gcollect() {
	GC_gcollect();
}

#else // !USE_GCMALLOC

unsigned long long X_BYTES_ALLOCATED = 0;

void x_mem_init() {
}

void* x_malloc(size_t size) {
	X_BYTES_ALLOCATED += size;
	return malloc(size);
}

void x_mem_gcollect() {
}

#endif // USE_GCMALLOC

