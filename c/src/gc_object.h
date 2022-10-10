/*
	Object-based garbage collection.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#ifndef __gcobject_h__
#define __gcobject_h__

#include "langtypes.h"

extern unsigned long long GCOBJ_OBJECTS_SINCE_COLLECT;

void init_gc_object();
Object *new_gc_object(unsigned char type);

void print_gc_object_stats();

void gc_object_collect();

// mark object AND all objects reachable from object
// ** DO NOT CALL unless called by gc to mark objects **
void gc_mark_object(Object *obj);

void print_all_gc_objects();

#endif // __gcobject_h__
