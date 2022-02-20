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

Object NULLOBJ; // default constructor creates the null object

// check that int is valid or throw exception
void checkIntOrFail(int i) {
	if(i > MAX_INT_31 || i < MIN_INT_31) {
		throw LangError("Integer overflow");
	}
}

Object newInt(int i) {
	checkIntOrFail(i);
	Object obj;
	obj.type = TYPE_INT;
	obj.data.i = i;
	return obj;
}

void Object::setInt(int i) { 
	checkIntOrFail(i);
	type = TYPE_INT; 
	data.i = i; 
}

Object newBool(bool b) {
	Object obj;
	obj.type = TYPE_BOOL;
	obj.data.b = b;
	return obj;
}

Object newLambda(ObjList *objlist) {
	Object obj;
	obj.type = TYPE_LAMBDA;
	obj.data.objlist = objlist;
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

Object newString(const char *s, size_t len, bool keepPointer) {
	Object obj;
	obj.type = TYPE_STRING;
	if(keepPointer) {
		obj.data.str = s;
	}
	else {
		obj.data.str = x_strndup(s, len);
	}
	return obj;
}

Object newString(const string& s) {
	return newString(s.c_str(), s.length());
}

// like above
Object newSymbol(const char *s, size_t len, bool keepPointer) {
	Object obj;
	obj.type = TYPE_SYMBOL;
	if(keepPointer) {
		obj.data.str = s;
	}
	else {
		obj.data.str = x_strndup(s, len);
	}
	return obj;
}

Object newSymbol(const string& s) {
	return newSymbol(s.c_str(), s.length());
}

bool Object::opEqual(const Object &other) {
	switch(type) {
		case TYPE_NULL: return other.isNull();
		case TYPE_INT: return (other.type == TYPE_INT && other.data.i == data.i) ||
							(other.type == TYPE_FLOAT && other.data.d == data.i);
		case TYPE_FLOAT: return (other.type == TYPE_FLOAT && other.data.d == data.d) ||
							(other.type == TYPE_INT && other.data.i == data.d);
		case TYPE_BOOL: return other.type == TYPE_BOOL && other.data.b == data.b;
		case TYPE_LAMBDA: throw LangError("Lambdas cannot be compared with ==");
		case TYPE_MEMARRAY: throw LangError("Memory arrays cannot be compared with ==");
		case TYPE_STRING: return other.type == TYPE_STRING && !strcmp(data.str,other.data.str);
		case TYPE_SYMBOL: return other.type == TYPE_SYMBOL && !strcmp(data.str,other.data.str);
		default: return false;
	}
}

bool isNumber(const Object &a) {
	return a.type == TYPE_INT || a.type == TYPE_FLOAT;
}

// get int or float as double
double castFloat(const Object &a) { 
	if(a.type == TYPE_INT) { 
		return (double)(a.data.i); 
	}
	else if(a.type == TYPE_FLOAT) {
		return a.data.d;
	}
	else {
		throw LangError("Cannot cast to float: " + a.fmtStackPrint());
	}
}

Object Object::opAdd(const Object &other) {
	switch(type) {
		case TYPE_NULL: break;
		case TYPE_INT:
			if(other.type == TYPE_INT) {
				return newInt(data.i + other.data.i);
			}
			else if(other.type == TYPE_FLOAT) {
				return newFloat(data.i + other.data.d);
			}
			else if(other.type == TYPE_MEMARRAY) {
				// can't directly reuse 'other' since offset and count are part of
				// the allocated object, but can share underlying array
				Object r = copyMemArray(other.asMemArray());
				r.asMemArray()->offset += data.i;
				return r;
			}
			break;
		case TYPE_FLOAT:
			if(other.type == TYPE_INT) {
				return newFloat(data.d + other.data.i);
			}
			else if(other.type == TYPE_FLOAT) {
				return newFloat(data.d + other.data.d);
			}
			break;
		case TYPE_BOOL: break;
		case TYPE_LAMBDA: break;
		case TYPE_MEMARRAY:
			if(other.type == TYPE_INT) {
				// can't directly reuse 'this' since offset and count are part of
				// the allocated object, but can share underlying array
				Object r = copyMemArray(asMemArray());
				r.asMemArray()->offset += other.data.i;
				return r;
			}
			break;
		// strings & symbols defined as immutable, so no changing this
		case TYPE_STRING:
			if(other.type == TYPE_STRING) {
				string s = this->data.str;
				s += other.data.str;
				return newString(s);
			}
			break;
		case TYPE_SYMBOL:
			if(other.type == TYPE_SYMBOL) {
				string s = this->data.str;
				s += other.data.str;
				return newSymbol(s);
			}
			break;

		default: break; // just to be explicit
	}
			
	throw LangError("Bad operands for +: " + this->fmtDisplay() + " & " + other.fmtDisplay());
}

Object Object::opSubtract(const Object &other) {
	if(type == TYPE_INT && other.type == TYPE_INT) {
		return newInt(data.i - other.data.i);
	}
	else if(isNumber(*this) && isNumber(other)) {
		return newFloat(castFloat(*this) - castFloat(other));
	}
	throw LangError("Bad operands for -: " + this->fmtDisplay() + " & " + other.fmtDisplay());
}

Object Object::opMul(const Object &other) {
	if(type == TYPE_INT && other.type == TYPE_INT) {
		return newInt(data.i * other.data.i);
	}
	else if(isNumber(*this) && isNumber(other)) {
		return newFloat(castFloat(*this) * castFloat(other));
	}
	throw LangError("Bad operands for -: " + this->fmtDisplay() + " & " + other.fmtDisplay());
}

Object Object::opDivide(const Object &other) {
	if(isNumber(*this) && isNumber(other)) {
		double denom = castFloat(other);
		if(denom == 0) {
			throw LangError("Divide by zero");
		}
		return newFloat(castFloat(*this) / denom);
	}
	throw LangError("Bad operands for /: " + this->fmtDisplay() + " & " + other.fmtDisplay());
}

Object Object::opLength() {
	switch(type) {
		case TYPE_STRING:
		case TYPE_SYMBOL:
			return newInt(strlen(data.str));
		case TYPE_MEMARRAY:
			return newInt(data.memarray->count);
	}
	throw LangError("'length' not supported for object: " + fmtStackPrint());
}

int FLOAT_PRECISION = 17;

string Object::fmtDisplay() const {
	switch(type) {
		case TYPE_NULL: return "<null>";
		case TYPE_INT: return to_string(data.i);
		case TYPE_FLOAT: 
		{
			char buf[40];
			snprintf(buf, 39, "%.*g", FLOAT_PRECISION, data.d);
			return string(buf);
		}
		case TYPE_BOOL: return data.b ? "true" : "false";
		// these two are not meant to be printed, but can be shown in stack
		case TYPE_LAMBDA: throw LangError("Lambdas are not printable");
		case TYPE_MEMARRAY: throw LangError("MemArray is not printable");
		case TYPE_STRING: return string(data.str);
		// symbols should not normally be printed by programs, so they get
		// a ' to differentiate them from strings
		case TYPE_SYMBOL: return "'" + string(data.str);
		default: throw LangError("repr not implemented for this object");
	}
}

string Object::fmtStackPrint() const {
	switch(type) {
		case TYPE_NULL: return "<null>";
		case TYPE_INT: return to_string(data.i);
		case TYPE_FLOAT: 
		{
			char buf[40];
			// include # so type is clear
			snprintf(buf, 39, "#%.*g", FLOAT_PRECISION, data.d);
			return string(buf);
		}
		case TYPE_BOOL: return data.b ? "true" : "false";
		case TYPE_LAMBDA: return "<lambda>";
		case TYPE_MEMARRAY: return "var:" + to_string(data.memarray->count) + ":" + to_string(data.memarray->offset);
		// add " .. " so its clear on stack that it is a string
		case TYPE_STRING: return "\"" + string(data.str) + "\"";
		// opposite of above -- since strings get " .. " then i don't get a ' here
		case TYPE_SYMBOL: return data.str;
		default: throw LangError("repr not implemented for this object");
	}
}