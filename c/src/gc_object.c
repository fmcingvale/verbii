/*
	Object-based garbage collection.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "gc_object.h"
#include "interpreter.h"
#include "util.h"
#include <stdio.h>

#if defined(USE_GC_OBJECT)

double GC_OBJECT_TOTAL_COLLECT_TIME = 0;
uint64_t GC_OBJECT_TOTAL_COLLECTIONS = 0;

// newly allocated objects go into GEN1 and then are moved into later
// generations the longer they remain referenced
static Object *GEN1_HEAD = NULL;

static Object *GEN2_HEAD = NULL;

unsigned long long GCOBJ_OBJECTS_SINCE_COLLECT = 0;

void init_gc_object() {
	// HEAD objects are always dummy nodes to simplify rest of code
	GEN1_HEAD = (Object*)x_malloc(sizeof(Object));
	GEN1_HEAD->gc_next = NULL;
	GEN2_HEAD = (Object*)x_malloc(sizeof(Object));
}

// head must be a fake head node (GEN*_HEAD)
static void gc_insert_object(Object *head, Object *obj) {
	// insert at head -- ordering doesn't matter and this is much easier than
	// having to keep track of the list tail
	obj->gc_next = head->gc_next;
	head->gc_next = obj;
}

static unsigned long long count_nodelist_length(Object *head) {
	int nr=0;
	while(head->gc_next) { // head is fake so head->next is first
		++nr;
		head = head->gc_next;
	}
	return nr;
}

static void set_all_marks(Object *head, uint8_t val) {
	while(head->gc_next) {
		head->gc_next->gc_mark = val;
		head = head->gc_next;
	}
}

// set marks on all objects that can be reached FROM obj (does NOT mark obj itself)
static void mark_reachable_from(Object *obj) {
	switch(obj->type) {
		case TYPE_LAMBDA:
		case TYPE_BOUND_LAMBDA:
			// mark the list and then all its reachable objects
			gc_mark_object(obj->data.lambda->list);
			// mark the callframe, if set
			if(obj->data.lambda->outer)
				gc_mark_object(obj->data.lambda->outer);
			
			break;

		case TYPE_LIST:
			for(int i=0; i<List_length(obj); ++i)
				gc_mark_object(List_get(obj,i));				
	
			break;

		case TYPE_DICT: {
			ObjDictEntry *ent;
			// mark all objects in dictionary
			for(ent=obj->data.objdict; ent != NULL; ent = ent->hh.next)
				gc_mark_object(ent->obj);				
		}
		break;

		case TYPE_CALLFRAMEDATA: {
			for(int i=0; i<MAX_CALLFRAME_SLOTS; ++i)
				gc_mark_object(obj->data.framedata->data[i]);

			if(obj->data.framedata->outer)
				gc_mark_object(obj->data.framedata->outer);
		}
		break;
	}
}	

void gc_mark_object_no_subobjects(Object *obj) {
	if(obj->gc_mark)
		// already marked on this cycle, return immediately to avoid loops
		// and also don't want to increase gc_count more than once per cycle
		return; 
	obj->gc_mark = 1;
	obj->gc_count += 1;
}

void gc_mark_object(Object *obj) {
	if(obj->gc_mark)
		// same notes as above
		return; 
	obj->gc_mark = 1;
	obj->gc_count += 1;
	mark_reachable_from(obj);
}

static unsigned long long count_marks(Object *head, uint8_t val) {
	unsigned long long count = 0;
	for(Object *node=head->gc_next; node; node = node->gc_next) {
		if(node->gc_mark == val)
			++count;
	}
	return count;
}

// can pass promote_tail=NULL to skip promotions
static void sweep_objects(Object *head, uint8_t promote_count, Object *promote_head) {
	// since there is only a .gc_next, always operate on head->gc_next
	while(head->gc_next) {
		if(head->gc_next->gc_mark == 0) {
			// unreachable object, remove
			Object *obj = head->gc_next;
			head->gc_next = head->gc_next->gc_next;
			// free extra data associated with obj
			switch(obj->type) {
				case TYPE_STRING: freeobj_string(obj); break;
				case TYPE_SYMBOL: freeobj_symbol(obj); break;
				case TYPE_LIST: freeobj_list(obj); break;
				case TYPE_DICT: freeobj_dict(obj); break;
				// can use same for both
				case TYPE_LAMBDA:
				case TYPE_BOUND_LAMBDA: freeobj_lambda(obj); break;
				case TYPE_CALLFRAMEDATA: freeobj_callframedata(obj); break;
			}
			// ... and then free obj
			//printf("FREEING OBJECT: %s\n", fmtStackPrint(obj));
			++DEALLOCS_BY_TYPE[obj->type];
			x_free(obj);
			
		}
		else if(head->gc_count >= promote_count && promote_head != NULL) {
			// remove from current list
			Object *obj = head->gc_next;
			head->gc_next = head->gc_next->gc_next;
			// add to new list
			gc_insert_object(promote_head, obj);
			// reset count
			obj->gc_count = 0;
		}
		else 
			head = head->gc_next;
	}
}

void gc_object_collect() {
	double t0 = current_system_cpu_time();
	// clear marks on objects that I want to test for being reachable
	set_all_marks(GEN1_HEAD, 0);
	set_all_marks(GEN2_HEAD, 0);
	// now mark the live objects

	// some objects are only reachable from langtypes/interpreter internal references
	langtypes_mark_reachable_objects();
	interpreter_mark_reachable_objects();
	
	// mark objects reachable from other GEN1 objects
	for(Object *node = GEN1_HEAD->gc_next; node; node = node->gc_next)
		mark_reachable_from(node);
	
		for(Object *node = GEN2_HEAD->gc_next; node; node = node->gc_next)
		mark_reachable_from(node);
	
	// promote older objects and remove unreachable objects
	sweep_objects(GEN1_HEAD, 2, GEN2_HEAD);
		sweep_objects(GEN2_HEAD, 0, NULL);
	double t1 = current_system_cpu_time();
	
	GC_OBJECT_TOTAL_COLLECT_TIME += (t1-t0);

	GCOBJ_OBJECTS_SINCE_COLLECT = 0; // reset stats
	++GC_OBJECT_TOTAL_COLLECTIONS;
}

static void gc_print_object_list(Object *head) {
	for(Object *node=head->gc_next; node; node = node->gc_next) {
		printf("  %1d %4d %s\n", node->gc_mark, node->gc_count, fmtStackPrint(node));
		head = head->gc_next;
	}
}

void print_all_gc_objects() {
	printf("GEN1 objects:\n");
	gc_print_object_list(GEN1_HEAD);
	printf("GEN2 objects:\n");
	gc_print_object_list(GEN2_HEAD);
}

void print_gc_object_stats() {
	printf("  number of collections: %lu\n", GC_OBJECT_TOTAL_COLLECTIONS);
	printf("  total collection time:   %lf\n", GC_OBJECT_TOTAL_COLLECT_TIME);
	printf("  GEN1 objects: %llu\n", count_nodelist_length(GEN1_HEAD));
	printf("  GEN2 objects: %llu\n", count_nodelist_length(GEN2_HEAD));
}

void shutdown_gc_object() {
	printf("Shutting down gc-object: %llu %llu\n", count_nodelist_length(GEN1_HEAD), count_nodelist_length(GEN2_HEAD));
	// mark all objects as non-reachable
	set_all_marks(GEN1_HEAD, 0);
	set_all_marks(GEN2_HEAD, 0);
	// .. and collect them
	sweep_objects(GEN1_HEAD, 0, NULL);
	sweep_objects(GEN2_HEAD, 0, NULL);
	printf("After 1 collection: %llu %llu\n", count_nodelist_length(GEN1_HEAD), count_nodelist_length(GEN2_HEAD));

	// remove head
	x_free(GEN1_HEAD);
	x_free(GEN2_HEAD);
}

Object *new_gc_object(unsigned char type) {
	++ALLOCS_BY_TYPE[type]; // stats
	Object *obj = (Object*)x_malloc(sizeof(Object));
	obj->type = type;
	// new objects start in GEN1
	gc_insert_object(GEN1_HEAD, obj);
	
	++GCOBJ_OBJECTS_SINCE_COLLECT;

	return obj;
}

#else

// not using GC-OBJECT - provide no-op replacements
void init_gc_object() { }

// ok, this is not a no-op, but just a x_malloc without the gc-object stuff
Object *new_gc_object(unsigned char type) {
	++ALLOCS_BY_TYPE[type]; // stats
	Object *obj = (Object*)x_malloc(sizeof(Object));
	obj->type = type;
	return obj;
}

void print_gc_object_stats() { }
void gc_object_collect() { }
void gc_mark_object(Object *obj) { }
void print_all_gc_objects() { }

#endif // USE_GC_OBJECT