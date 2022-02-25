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
#include <string.h>
#include <string>
#include <vector>

const unsigned char TYPE_NULL = 0;
const unsigned char TYPE_INT = 1;
const unsigned char TYPE_BOOL = 2;
const unsigned char TYPE_LAMBDA = 3;
const unsigned char TYPE_MEMARRAY = 4;
const unsigned char TYPE_FLOAT = 5;
const unsigned char TYPE_STRING = 6;
const unsigned char TYPE_SYMBOL = 7;
const unsigned char TYPE_LIST = 8;

class Object;

typedef std::vector<Object> ObjList;

// set this to control how many digits are printed (max is 17)
// (this is TOTAL digits, not digits after the decimal ... so 'g' format for printf)
extern int FLOAT_PRECISION;

struct MemoryArray {
	//Object *array;
	//int count;
	ObjList *list;
	int offset; // when code does pointer math, this is adjusted
};

// this is intended to be a POD type, so no non-default constructors, destructors, base classes,
// and small enough to pass as value -- any large parts will be stored in pointers
class Object {
	public:
	// not normally used -- use the new* functions below to create Objects
	// this is only used to create the NULLOBJ
	Object() { type=TYPE_NULL; data.str = NULL; }

	// type checking
	bool isNull() const { return type == TYPE_NULL; }
	bool isInt() const { return type == TYPE_INT; }
	bool isBool() const { return type == TYPE_BOOL; }
	bool isLambda() const { return type == TYPE_LAMBDA; }
	bool isMemArray() const { return type == TYPE_MEMARRAY; }
	bool isFloat() const { return type == TYPE_FLOAT; }
	bool isString() const { return type == TYPE_STRING; }
	bool isSymbol() const { return type == TYPE_SYMBOL; }
	// convenience -- test if symbol AND equal to given string
	// if n>0 then only require that many chars to match
	bool isSymbol(const char *s, int n=0) const 
		{ return type == TYPE_SYMBOL &&
			((n == 0 && !strcmp(data.str,s)) ||
			(n > 0 && !strncmp(data.str, s, n))); }
	bool isList() const { return type == TYPE_LIST; }
			
	// get value (make sure to check first)
	unsigned int asInt() const { return data.i; }
	bool asBool() const { return data.b; }
	ObjList* asLambda() const { return data.objlist; };
	MemoryArray* asMemArray() const { return data.memarray; }
	double asFloat() const { return data.d; }
	ObjList *asList() const { return data.objlist; }
	 
	const char *asString() const { return data.str; }
	const char *asSymbol() const { return data.str; }
	
	// setters to change value of object, i.e. for reusing object
	void setInt(int i);

	// '==' builtin (exact match except allows for int==float)
	bool opEqual(const Object &other);
	// '>' builtin (int, float, string, symbol)
	bool opGreater(const Object &other);
	// '+' builtin (int, float, memarray, strings, symbols, lists)
	Object opAdd(const Object &other);
	// '-' builtin (int, float)
	Object opSubtract(const Object &other);
	// '*' builtin (int, float)
	Object opMul(const Object &other);
	// '/' builtin (int, float) - *ALWAYS* floating point result - use divmod for floor-divide behavior on ints
	Object opDivide(const Object &other);
	// '/mod' (ints)
	Object opDivMod(const Object &other);
	// 'length' word (string, symbol, array, list)
	Object opLength();
	// 'slice' operation - start at index, get nr items, or -1 for rest
	// index/length out of bounds is never an error - returns empty object in worst case
	// negative indexes count from end (where -1 is last item)
	// for: strings, symbols, lists, arrays
	//
	// NOTE: slice are the same type as the original object -- so a 1-length slice of a list is still a list.
	// use unmake when you need the contents in the original type.
	Object opSlice(int index, int nr);

	// get string representation of object for printing to output
	// (like would be displayed in normal program output)
	std::string fmtDisplay() const;
	// get string representation of object for printing a stack
	// (generally more verbose to convey extra type information)
	std::string fmtStackPrint() const;
	
	// do NOT read/write directly, always use functions above
	unsigned char type;
	union {
		int i; // ints and lamda (index into LAMBDAS)
		bool b;
		MemoryArray *memarray;
		ObjList *objlist; // for lambdas & lists
		double d;
		const char *str; // strings & symbols, immutable
	} data;
};

// single NULL object
extern Object NULLOBJ;

// these return Null objects on parsing error
Object parseInt(const std::string &);
Object parseFloat(const std::string &);

Object newNull();
Object newInt(int i);
Object newBool(bool b);
Object newLambda(ObjList *objlist);
// allocates array and sets all objects to int with value 0
Object newMemArray(int count, int offset);
// make a copy of the given array, sharing its array, so it
// can have its own offset without affecting original. 
// offset is set the same as memarray.
Object copyMemArray(MemoryArray *memarray);
Object newFloat(double d);
// copies s, unless keepPointer is true, in which case it 
// takes ownership of s (and len is ignored)
Object newString(const char *s, size_t len, bool keepPointer=false);
Object newString(const std::string& );
// like above
Object newSymbol(const char *s, size_t len, bool keepPointer=false);
Object newSymbol(const std::string& );

Object newList(); // always makes empty list
Object newList(ObjList *); // wraps existing list, does NOT copy

