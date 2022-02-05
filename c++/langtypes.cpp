/*
	LangTypes - type system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "langtypes.hpp"
#include "errors.hpp"
using namespace std;

// integers only allowed to use 31 bits, to be portable across host languages
const int MAX_INT_31 = (1<<30) - 1;
const int MIN_INT_31 = -MAX_INT_31;

Object newInt(int i) {
	if(i > MAX_INT_31 || i < MIN_INT_31) {
		throw LangError("Integer overflow");
	}
	Object obj;
	obj.type = TYPE_INT;
	obj.data.i = i;
	return obj;
}

Object newBool(bool b) {
	Object obj;
	obj.type = TYPE_BOOL;
	obj.data.b = b;
	return obj;
}

Object newLambda(int index) {
	Object obj;
	obj.type = TYPE_LAMBDA;
	obj.data.i = index;
	return obj;
}

Object newMemArray(int count, int offset) {
	Object *array = (Object*)GC_malloc(count*sizeof(Object));
	MemoryArray *memarray = (MemoryArray*)GC_malloc(sizeof(MemoryArray));

	memarray->array = array;
	memarray->count = count;
	memarray->offset = offset;

	Object obj;
	obj.type = TYPE_MEMARRAY;
	obj.data.memarray = memarray;
	return obj;
}

Object copyMemArray(MemoryArray *memarray) {
	MemoryArray *arraycopy = (MemoryArray*)GC_malloc(sizeof(MemoryArray));
	arraycopy->array = memarray->array;
	arraycopy->count = memarray->count;
	arraycopy->offset = memarray->offset;

	Object obj;
	obj.type = TYPE_MEMARRAY;
	obj.data.memarray = arraycopy;
	return obj;
}

string Object::repr() const {
	switch(type) {
		case TYPE_VOID: throw LangError("Got VOID object, something is wrong");
		case TYPE_INT: return to_string(data.i);
		case TYPE_BOOL: return data.b ? "true" : "false";
		case TYPE_LAMBDA: return "<lambda>";
		default: throw LangError("repr not implemented for this object");
	}
}