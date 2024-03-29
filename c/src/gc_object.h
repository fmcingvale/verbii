/*
	Object-based garbage collection.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#ifndef __gcobject_h__
#define __gcobject_h__

#include "langtypes.h"

#if defined(USE_GC_OBJECT)
extern unsigned long long GCOBJ_OBJECTS_SINCE_COLLECT;
extern double GC_OBJECT_TOTAL_COLLECT_TIME;
extern uint64_t GC_OBJECT_TOTAL_COLLECTIONS;
#endif

void init_gc_object();
Object *new_gc_object(unsigned char type);

void print_gc_object_stats();

void gc_object_collect();

// mark object AND all objects reachable from object
// ** DO NOT CALL unless called by gc to mark objects **
void gc_mark_reachable(Object *obj);

// mark/unmark object as non-collectable (NON recursive)
void gc_mark_object_keep_non_recursive(Object *obj);
void gc_clear_object_keep_non_recursive(Object *obj);

// special case - mark obj but DO NOT mark any contained objects
void gc_mark_object_no_subobjects(Object *obj);

void print_all_gc_objects();

// attempt to free all remaining objects -- do not use this module
// again after this, without calling init_gc_object()
void shutdown_gc_object();

#endif // __gcobject_h__
