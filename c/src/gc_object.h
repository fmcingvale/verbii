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
#endif

void init_gc_object();
Object *new_gc_object(unsigned char type);

void print_gc_object_stats();

void gc_object_collect();

// mark object AND all objects reachable from object
// ** DO NOT CALL unless called by gc to mark objects **
void gc_mark_object(Object *obj);

// special case - mark obj but DO NOT mark any contained objects
void gc_mark_object_no_subobjects(Object *obj);

void print_all_gc_objects();

#endif // __gcobject_h__
