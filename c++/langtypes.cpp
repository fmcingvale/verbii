/*
	LangTypes - type system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "langtypes.hpp"
#include "errors.hpp"
#include "xmalloc.hpp"
#include <sstream>
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

void Object::setInt(int i) { 
	if(i > MAX_INT_31 || i < MIN_INT_31) {
		throw LangError("Integer overflow");
	}
	type = TYPE_INT; 
	data.i = i; 
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
	Object *array = (Object*)x_malloc(count*sizeof(Object));
	MemoryArray *memarray = (MemoryArray*)x_malloc(sizeof(MemoryArray));

	// set all to int=0 as default value
	for(int i=0; i<count; ++i) {
		array[i].type = TYPE_INT;
		array[i].data.i = 0;
	}

	memarray->array = array;
	memarray->count = count;
	memarray->offset = offset;

	Object obj;
	obj.type = TYPE_MEMARRAY;
	obj.data.memarray = memarray;
	return obj;
}

Object copyMemArray(MemoryArray *memarray) {
	MemoryArray *arraycopy = (MemoryArray*)x_malloc(sizeof(MemoryArray));
	arraycopy->array = memarray->array;
	arraycopy->count = memarray->count;
	arraycopy->offset = memarray->offset;

	Object obj;
	obj.type = TYPE_MEMARRAY;
	obj.data.memarray = arraycopy;
	return obj;
}

Object newFloat(double d) {
	Object obj;
	obj.type = TYPE_FLOAT;
	obj.data.d = d;
	return obj;
}

string Object::repr() const {
	switch(type) {
		case TYPE_VOID: throw LangError("Got VOID object, something is wrong");
		case TYPE_INT: return to_string(data.i);
		case TYPE_FLOAT: 
		{
			char buf[40];
			snprintf(buf, 39, "%.17lf", data.d);
			return string(buf);
		}
		case TYPE_BOOL: return data.b ? "true" : "false";
		case TYPE_LAMBDA: return "<lambda>";
		case TYPE_MEMARRAY: return "var:" + to_string(data.memarray->count) + ":" + to_string(data.memarray->offset);
		default: throw LangError("repr not implemented for this object");
	}
}