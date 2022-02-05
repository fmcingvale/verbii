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

string Object::repr() const {
	switch(type) {
		case TYPE_VOID: return "<<VOID>>"; // should never happen
		case TYPE_INT: return to_string(data.i);
		case TYPE_BOOL: return data.b ? "true" : "false";
		case TYPE_LAMBDA: return "<lambda>";
		default: throw LangError("repr not implemented for this object");
	}
}