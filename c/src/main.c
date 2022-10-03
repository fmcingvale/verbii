#include <stdio.h>
#include "langtypes.h"
#include "interpreter.h"
#include "native.h"

int main(void) {
	printf("Hello C!\n");
	init_object_system();
	init_interpreter();
	init_builtins();

	Object *i, *f, *b1, *b0, *L, *st, *sy;
	i = newInt(123);
	f = newFloat(81.125);
	b1 = newBool(1);
	b0 = newBool(0);
	st = newString("Hello world!", -1);
	sy = newSymbol("hello-symbol", -1);
	printf("int: %s\n", fmtStackPrint(i));
	printf("float: %s\n", fmtStackPrint(f));
	printf("bool, true: %s\n", fmtStackPrint(b1));
	printf("bool, false: %s\n", fmtStackPrint(b0));
	printf("string: %s\n", fmtStackPrint(st));
	printf("symbol: %s\n", fmtStackPrint(sy));

	L = newList();
	List_append(L, newInt(123));
	List_append(L, newFloat(567.8125));
	List_append(L, newBool(1));
	List_append(L, newBool(0));
	List_append(L, newVoid());
	List_append(L, newNull());
	printf("list: %s\n", fmtStackPrint(L));

	Object *dict = newDict();
	Dict_put(dict, "aaa", newInt(123));
	Dict_put(dict, "bbb", newFloat(1.25));
	Dict_put(dict, "ccc", newString("A string here",-1));
	Dict_put(dict, "ddd", newSymbol("A symbol",-1));
	Dict_put(dict, "eee", newBool(TRUE));
	Dict_put(dict, "fff", L);
	printf("dict: %s\n", fmtStackPrint(dict));

	printf("STACK: [ %s ]\n", reprStack());
	push(newInt(456));
	push(newFloat(27.350));
	push(newString("A string here",-1));
	push(newSymbol("A-symbol-here",-1));
	push(newBool(TRUE));
	push(newBool(FALSE));
	push(newVoid());
	push(newNull());
	push(dict);
	push(L);
	push(newLambda(L));
	printf("STACK: [ %s ]\n", reprStack());

	Object *prog = newList();
	List_append(prog,newInt(11));
	List_append(prog,newSymbol("make-list",-1));
	run(prog);

	printf("STACK: [ %s ]\n", reprStack());

	
}