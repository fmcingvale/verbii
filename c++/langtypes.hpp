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
const unsigned char TYPE_FLOAT = 4;
const unsigned char TYPE_STRING = 5;
const unsigned char TYPE_SYMBOL = 6;
const unsigned char TYPE_LIST = 7;
// verbii's idea of a closure is little different that in other languages -- mainly
// that it does not capture by name (since verbii functions do not have named args)
// and doesn't capture any outer scope (since, again, there are no names to capture).
// instead it binds an object and a function together, thereby giving the function
// a state. instead of inventing a new word for "function with state", i'm calling
// them closures since they are at least similar in concept.
const unsigned char TYPE_CLOSURE = 8;
// unlike TYPE_NULL, which gets pushed as an immediate when encountered,
// type VOID is never pushed and it is an error in most cases to pass
// it as a value. it is only used internally for a return type meaning
// "nothing, not even null"
const unsigned char TYPE_VOID = 9;
class Object;

typedef std::vector<Object> ObjList;

// set this to control how many digits are printed (max is 17)
// (this is TOTAL digits, not digits after the decimal ... so 'g' format for printf)
extern int FLOAT_PRECISION;

class Closure;

// this is intended to be a POD type, so no non-default constructors, destructors, base classes,
// and small enough to pass as value -- any large parts will be stored in pointers
class Object {
	public:
	// not normally used -- use the new* functions below to create Objects
	// this is only used to create the NULLOBJ
	Object() { type=TYPE_NULL; data.str = NULL; }

	// type checking
	bool isNull() const { return type == TYPE_NULL; }
	bool isVoid() const { return type == TYPE_VOID; }
	bool isInt() const { return type == TYPE_INT; }
	bool isBool() const { return type == TYPE_BOOL; }
	bool isLambda() const { return type == TYPE_LAMBDA; }
	bool isFloat() const { return type == TYPE_FLOAT; }
	bool isString() const { return type == TYPE_STRING; }
	bool isSymbol() const { return type == TYPE_SYMBOL; }
	bool isClosure() const { return type == TYPE_CLOSURE; }
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
	double asFloat() const { return data.d; }
	ObjList *asList() const { return data.objlist; }
	 
	const char *asString() const { return data.str; }
	const char *asSymbol() const { return data.str; }

	ObjList *asClosureFunc() const;
	Object asClosureState() const;

	// no as* function for Void since it is never supposed to be used

	// setters to change value of object, i.e. for reusing object
	void setInt(int i);

	// '==' builtin (exact match except allows for int==float)
	bool opEqual(const Object &other);
	// '>' builtin (int, float, string, symbol)
	bool opGreater(const Object &other);
	// '+' builtin (int, float, strings, symbols, lists)
	Object opAdd(const Object &other);
	// '-' builtin (int, float)
	Object opSubtract(const Object &other);
	// '*' builtin (int, float)
	Object opMul(const Object &other);
	// '/' builtin (int, float) - *ALWAYS* floating point result - use divmod for floor-divide behavior on ints
	Object opDivide(const Object &other);
	// '/mod' (ints)
	Object opDivMod(const Object &other);
	// 'length' word (string, symbol, list)
	Object opLength();
	// 'slice' operation - start at index, get nr items, or -1 for rest
	// index/length out of bounds is never an error - returns empty object in worst case
	// negative indexes count from end (where -1 is last item)
	// for: strings, symbols, lists
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
		ObjList *objlist; // for lambdas & lists
		double d;
		const char *str; // strings & symbols, immutable
		Closure *closure;
	} data;
};

class Closure {
	public:
	ObjList *objlist; // the function
	Object state; // the bound state
};


// single NULL object
extern Object NULLOBJ;
// single Void object
extern Object VOIDOBJ;

// these return Null objects on parsing error
Object parseInt(const std::string &);
Object parseFloat(const std::string &);

Object newNull();
Object newVoid();
Object newInt(int i);
Object newBool(bool b);
Object newLambda(ObjList *objlist);
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

Object newClosure(ObjList *, Object);
