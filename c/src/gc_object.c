/*
	Object-based garbage collection.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "gc_object.h"
#include "interpreter.h"
#include "util.h"

#if defined(USE_GC_OBJECT)

double GC_OBJECT_TOTAL_COLLECT_TIME = 0;

static Object *GEN1_HEAD = NULL;
static Object *GEN1_TAIL = NULL;

unsigned long long GCOBJ_OBJECTS_SINCE_COLLECT = 0;

void init_gc_object() {
	// HEAD objects are always dummy nodes to simplify rest of code
	GEN1_HEAD = (Object*)x_malloc(sizeof(Object));
	GEN1_TAIL = GEN1_HEAD;
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

static void gc_mark_all_live_objects() {
	//printf("** MARKING LIVE OBJECTS:\n");
	set_all_marks(GEN1_HEAD, 0);
	//printf("EXPECT ZERO: %llu\n", count_marks(GEN1_HEAD,1));

	langtypes_mark_reachable_objects();
	interpreter_mark_reachable_objects();
	for(Object *node = GEN1_HEAD->gc_next; node; node = node->gc_next)
		mark_reachable_from(node);

	//printf("LIVE OBJECTS: %llu\n", count_marks(GEN1_HEAD,1));
}
	
static void remove_unmarked_objects(Object *head) {
	while(head->gc_next) {
		if(head->gc_next->gc_mark == 0) {
			Object *obj = head->gc_next;
			head->gc_next = head->gc_next->gc_next;
			#if 1
			// free extra data associated with obj
			switch(obj->type) {
				case TYPE_STRING: freeobj_string(obj); break;
				case TYPE_SYMBOL: freeobj_symbol(obj); break;
				case TYPE_LIST: freeobj_list(obj); break;
				case TYPE_DICT: freeobj_dict(obj); break;
				case TYPE_LAMBDA:
				case TYPE_BOUND_LAMBDA: freeobj_lambda(obj); break;
				case TYPE_CALLFRAMEDATA: freeobj_callframedata(obj); break;
			}
			#endif
			// ... and then free obj
			//printf("FREEING OBJECT: %s\n", fmtStackPrint(obj));
			x_free(obj);
			
		}
		else 
			head = head->gc_next;
	}
}

void gc_object_collect() {
	double t0 = current_system_cpu_time();
	gc_mark_all_live_objects();
	remove_unmarked_objects(GEN1_HEAD);
	double t1 = current_system_cpu_time();
	
	GC_OBJECT_TOTAL_COLLECT_TIME += (t1-t0);

	GCOBJ_OBJECTS_SINCE_COLLECT = 0; // reset stats
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
}

void print_gc_object_stats() {
	printf("  GEN1 objects: %llu\n", count_nodelist_length(GEN1_HEAD));
}

Object *new_gc_object(unsigned char type) {
	Object *obj = (Object*)x_malloc(sizeof(Object));
	obj->type = type;
	obj->gc_next = NULL;
	// add into GEN1
	GEN1_TAIL->gc_next = obj;
	GEN1_TAIL = GEN1_TAIL->gc_next;

	++GCOBJ_OBJECTS_SINCE_COLLECT;

	return obj;
}

#else

// not using GC-OBJECT - provide no-op replacements
void init_gc_object() { }

// ok, this is not a no-op, but just a x_malloc without the gc-object stuff
Object *new_gc_object(unsigned char type) {
	Object *obj = (Object*)x_malloc(sizeof(Object));
	obj->type = type;
	return obj;
}

void print_gc_object_stats() { }
void gc_object_collect() { }
void gc_mark_object(Object *obj) { }
void print_all_gc_objects() { }

#endif // USE_GC_OBJECT