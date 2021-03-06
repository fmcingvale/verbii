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
#include <map>

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
// a void type which is differentiated from null.
// in general, void is used in eof-type situations but can be used in any context
// where a function needs to differentiate a return value of null from a
// return of 'nothing'. for example the verbii compiler needs to be able to do this
// so that null can be a literal. void can never be a literal since the same problem
// would recur that there would be no way to differentiate a parsed void from eof.
// void will only ever be a word, so it will always be parsed as a symbol. since void
// normally shouldn't be used in data anyways, i don't think that's a problem.
const unsigned char TYPE_VOID = 9;
const unsigned char TYPE_DICT = 10;

class Object;

typedef std::vector<Object> ObjList;
typedef std::map<std::string,Object> ObjDict;

// set this to control how many digits are printed (max is 17)
// (this is TOTAL digits, not digits after the decimal ... so 'g' format for printf)
extern int FLOAT_PRECISION;

// verbii's integer type
typedef int64_t VINT;

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
	bool isDict() const { return type == TYPE_DICT; }
			
	// get value (make sure to check first)
	VINT asInt() const { return data.i; }
	bool asBool() const { return data.b; }
	ObjList* asLambda() const { return data.objlist; };
	double asFloat() const { return data.d; }
	ObjList *asList() const { return data.objlist; }
	ObjDict *asDict() const { return data.objdict; }
	 
	const char *asString() const { return data.str; }
	const char *asSymbol() const { return data.str; }

	ObjList *asClosureFunc() const;
	Object asClosureState() const;

	// '==' builtin (exact match except allows for int==float)
	bool opEqual(const Object &other) const;
	// '>' builtin (int, float, string, symbol, lists)
	bool opGreater(const Object &other) const;
	// '+' builtin (int, float, strings, symbols, lists, dicts)
	Object opAdd(const Object &other) const;
	// '-' builtin (int, float)
	Object opSubtract(const Object &other) const;
	// '*' builtin (int, float)
	Object opMul(const Object &other) const;
	// '/' builtin (int, float) - *ALWAYS* floating point result - use divmod for floor-divide behavior on ints
	Object opDivide(const Object &other) const;
	// '/mod' (ints)
	Object opDivMod(const Object &other) const;
	// 'length' word (string, symbol, list, dict)
	Object opLength() const;
	// length as regular function
	int length() const;
	// 'slice' operation - start at index, get nr items, or -1 for rest
	// index/length out of bounds is never an error - returns empty object in worst case
	// negative indexes count from end (where -1 is last item)
	// for: strings, symbols, lists
	//
	// NOTE: slice are the same type as the original object -- so a 1-length slice of a list is still a list.
	// use unmake when you need the contents in the original type.
	Object opSlice(VINT index, VINT nr) const;

	// create a deepcopy of object
	// semantics: modifying a deepcopy of an object cannot change the original object.
	// implications: lists & dicts are the only modifiable object in verbii. therefore they
	//               are the only object that has to be deepcopied. references to immutable
	//               objects to not have to be deepcopied
	//
	// this is safe to call on ANY object, but any object except a list or dict will just return itself
	Object deepcopy() const;
	
	// get string representation of object for printing to output
	// (like would be displayed in normal program output)
	std::string fmtDisplay() const;
	// get string representation of object for printing a stack
	// (generally more verbose to convey extra type information)
	std::string fmtStackPrint() const;
	
	// do NOT read/write directly, always use functions above
	unsigned char type;
	union {
		VINT i; // ints
		bool b;
		ObjList *objlist; // for lambdas & lists
		ObjDict *objdict; // for dict
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
// this raises exception on error
Object parseBool(const std::string &);

Object newNull();
Object newVoid();
Object newInt(VINT i);
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

Object newDict(); // make an empty dict

Object newClosure(ObjList *, Object);

// to deepcopy just the ObjList portion of a Object
ObjList *deepcopy(ObjList *objlist);