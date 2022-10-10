// test gc_object things
#include "gc_object.h"
#include "xmalloc.h"
#include "native.h"
#include "interpreter.h"
#include "langtypes.h"

int main(int argc, char *argv[]) {
	
	x_mem_init();

	init_gc_object();
	init_builtins();
	init_object_system();

	init_interpreter();

	// ----------------------------------

	// create objects on stack (so will stay live)
	push(newInt(123));
	push(newFloat(8.125));
	push(newString("ABCDE",-1));
	push(newSymbol("XYZ",-1));
	Object *list = newList();
	List_append(list, newString("a",1));
	List_append(list, newString("b",1));
	List_append(list, newString("c",1));
	List_append(list, newString("d",1));
	push(list);

	//Object *dict = newDict();
	//Dict_put(dict, "aaa", newFloat(44));
	//Dict_put(dict, "bbb", newFloat(55));
	//Dict_put(dict, "ccc", newFloat(66));
	

	// ----------------------------------

	print_all_gc_objects();

	gc_object_collect();

	printf("** POPPING ALL OBJECTS\n");
	pop();
	pop();
	pop();
	pop();
	pop();

	print_all_gc_objects();

	gc_object_collect();

	printf("PUTTING OBJECTS INTO HEAP\n");
	int adr1 = heap_alloc(1);
	heap_set(adr1, newInt(1111));
	int adr2 = heap_alloc(1);
	heap_set(adr2, newInt(2222));
	int adr3 = heap_alloc(1);
	heap_set(adr3, newInt(3333));
	
	
	gc_object_collect();
	print_all_gc_objects();

	printf("CLEARING HEAP\n");
	heap_set(adr1, newNull());
	heap_set(adr2, newNull());
	heap_set(adr3, newNull());

	
	gc_object_collect();
	print_all_gc_objects();

	//print_stats();
}

