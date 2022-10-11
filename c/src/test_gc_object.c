// test gc_object things
#include "gc_object.h"
#include "xmalloc.h"
#include "native.h"
#include "interpreter.h"
#include "langtypes.h"
#include "errors.h"
#include <stdio.h>
#include <setjmp.h>

int main(int argc, char *argv[]) {
	
	if(setjmp(ERROR_JMP_BUF) == 1) {
		printf("%s\n", ERROR_MESSAGE);
		exit(1);
	}
			
	x_mem_init();

	init_gc_object();
	//init_builtins();
	init_object_system();

	//init_interpreter();
	Object *a = newString("string-A",-1);
	printf("string-a: %s\n", string_cstr(a));

	Object *b = newString("",-1);
	string_append(b,newString("string-B",-1));
	printf("string-b: %s\n", string_cstr(b));

	Object *c = newString("",-1);
	for(int i=0;i<26;++i)
		string_addchar(c,'A'+i);

	printf("uppcase alphabet: %s\n", string_cstr(c));

	Object *d = newString("",0);
	for(int i=0;i<26;++i)
		string_addchar(d,'a'+i);

	printf("lowercase alphabet: %s\n", string_cstr(d));

	Object *e = newString("",0);
	for(int i=0;i<26;++i) {
		string_addchar(e,'A'+i);
		string_addchar(e,'a'+i);
	}

	printf("upper+lower: %s\n", string_cstr(e));

	Object *f = newString("",0);
	string_append(f, c);
	string_append(f, d);
	string_append(f, e);
	
	printf("all 3: %s\n", string_cstr(f));

	Object *g = newString("",0);
	string_printf(g, "#1: %s, #2: %s, #3: %s", string_cstr(c), string_cstr(d), string_cstr(e));
	printf("all 3, numbered: %s\n", string_cstr(g));

#if 0
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

#endif
	printf("  allocations by type (deallocations in parens):\n");
	unsigned long tot_objects = 0;
	for(int i=0; i<TYPE_LAST_PLUS_1; ++i) {
		printf("    %15s = %12lu (%12lu %6.2lf%%)\n", TYPE_TO_NAME[i], ALLOCS_BY_TYPE[i], DEALLOCS_BY_TYPE[i],
					(100.0*DEALLOCS_BY_TYPE[i])/ALLOCS_BY_TYPE[i]);
		tot_objects += ALLOCS_BY_TYPE[i];
	}
	printf("  total objects: %lu\n", tot_objects);

	//print_all_gc_objects();
	printf("%llu bytes alloc\n", X_BYTES_ALLOCATED);
	printf("%llu bytes freed\n", X_BYTES_FREED);
	
	printf("Collect ...\n");
	gc_object_collect();
	//print_all_gc_objects();

	printf("%llu bytes alloc\n", X_BYTES_ALLOCATED);
	printf("%llu bytes freed\n", X_BYTES_FREED);

	printf("Shutdown ...\n");
	shutdown_gc_object();

	printf("%llu bytes alloc\n", X_BYTES_ALLOCATED);
	printf("%llu bytes freed\n", X_BYTES_FREED);

	//print_stats();
}

