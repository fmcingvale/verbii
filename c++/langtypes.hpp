/*
	LangTypes - type system. Object is designed to be small (currently the same size as
	an integer) so there shouldn't be TOO much penalty to using Object vs integers. originally
	i was using tagged integers to represent ALL objects, but as I moved towards wanting to
	add garbage collection that seemed like it was going to cause problems (i.e hiding/obfuscating
	pointers meant i might have to write my own gc ... violating the simplicity rule ... so I
	switched to a collectable object class instead)

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once
#include "reader.hpp"
#include "xmalloc.hpp"
#include <string.h>
#include <string>

const unsigned char TYPE_VOID = 0;
const unsigned char TYPE_INT = 1;
const unsigned char TYPE_BOOL = 2;
const unsigned char TYPE_LAMBDA = 3;
const unsigned char TYPE_MEMARRAY = 4;
const unsigned char TYPE_FLOAT = 5;

class Object;

struct MemoryArray {
	Object *array;
	int count;
	int offset; // when code does pointer math, this is adjusted
};

// this is intended to be a POD type, so no constructors, destructors, base classes,
// and small enough to pass as value -- any large parts will be stored in pointers
class Object {
	public:
	// use one of the new* functions (below) to create Objects

	// type checking
	bool isInt() const { return type == TYPE_INT; }
	bool isBool() const { return type == TYPE_BOOL; }
	bool isLambda() const { return type == TYPE_LAMBDA; }
	bool isMemArray() const { return type == TYPE_MEMARRAY; }
	bool isFloat() const { return type == TYPE_FLOAT; }

	// get value (make sure to check first)
	unsigned int asInt() const { return data.i; }
	bool asBool() const { return data.b; }
	int asLambdaIndex() const { return data.i; };
	MemoryArray* asMemArray() { return data.memarray; }
	double asFloat() { return data.d; }
	
	// setters to change value of object, i.e. for reusing object
	void setInt(int i);

	// get printable version of object
	std::string repr() const;

	// do NOT read/write directly, always use functions above
	unsigned char type;
	union {
		int i; // ints and lamda (index into LAMBDAS)
		bool b;
		MemoryArray *memarray;
		double d;
	} data;
};

Object newInt(int i);
Object newBool(bool b);
Object newLambda(int index);
// allocates array and sets all objects to int with value 0
Object newMemArray(int count, int offset);
// make a copy of the given array, sharing its array, so it
// can have its own offset without affecting original. 
// offset is set the same as memarray.
Object copyMemArray(MemoryArray *memarray);
Object newFloat(double d);

