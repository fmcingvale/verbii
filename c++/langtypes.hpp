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
#include "gc_cpp.h"
#include <string.h>
#include <string>

const int TYPE_VOID = 0;
const int TYPE_INT = 1;
const int TYPE_BOOL = 2;
const int TYPE_LAMBDA = 3;

// this is intended to be small enough to pass as value -- the
// large parts are in pointers
class Object : public gc {
	public:
	// use one of the new* functions to create Objects, not this constructor
	Object() { type = TYPE_VOID; memset(&data, 0, sizeof(data)); }
	~Object() { memset(&data, 0, sizeof(data)); } // help gc by clearing pointer

	friend Object newInt(int i);
	friend Object newBool(bool b);
	friend Object newLambda(int index); // index into LAMBDAS

	bool isInt() const { return type == TYPE_INT; }
	bool isBool() const { return type == TYPE_BOOL; }
	bool isLambda() const { return type == TYPE_LAMBDA; }

	unsigned int asInt() const { return data.i; }
	bool asBool() const { return data.i == 0 ? false : true; }
	int asLambdaIndex() const { return data.i; };

	std::string repr() const;

	protected:
	int type;
	union {
		int i; // ints and lamda (index into LAMBDAS)
		bool b;
	} data;
};

Object newInt(int i);
Object newBool(bool b);
Object newLambda(int index);

