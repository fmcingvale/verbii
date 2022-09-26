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
#include "xmalloc.hpp"

const unsigned char TYPE_NULL = 0;
const unsigned char TYPE_INT = 1;
const unsigned char TYPE_BOOL = 2;
const unsigned char TYPE_LAMBDA = 3;
const unsigned char TYPE_FLOAT = 4;
const unsigned char TYPE_STRING = 5;
const unsigned char TYPE_SYMBOL = 6;
const unsigned char TYPE_LIST = 7;
// a void type which is differentiated from null.
// in general, void is used in eof-type situations but can be used in any context
// where a function needs to differentiate a return value of null from a
// return of 'nothing'. for example the verbii compiler needs to be able to do this
// so that null can be a literal. void can never be a literal since the same problem
// would recur that there would be no way to differentiate a parsed void from eof.
// void will only ever be a word, so it will always be parsed as a symbol. since void
// normally shouldn't be used in data anyways, i don't think that's a problem.
const unsigned char TYPE_VOID = 8;
const unsigned char TYPE_DICT = 9;
// 'version 2' closure
const unsigned char TYPE_BOUND_LAMBDA = 10;
// interpreter opcodes
const unsigned char TYPE_OPCODE = 11;

class Object;

typedef std::vector<Object> ObjList;
typedef std::map<std::string,Object> ObjDict;

// set this to control how many digits are printed (max is 17)
// (this is TOTAL digits, not digits after the decimal ... so 'g' format for printf)
extern int FLOAT_PRECISION;

// verbii's integer type
typedef int64_t VINT;

class CallFrameData;

// a Lambda that is bound to a CallFrameData, thereby giving it access
// the all the frame data of its outer context
class BoundLambda {
	public:
	ObjList *objlist; // store list directly instead of lambda Object
	CallFrameData *outer;
};

class StringData {
	public:
	// if keepPointer is false, makes a copy of bytes
	// else, keeps pointer instead of copying
	// ** NOTE ** when setting keepPointer=true, bytes must already
	// have a NULL at end that is NOT included in len
	StringData(const char *bytes, int len, bool keepPointer) {
		if(keepPointer) {
			_str = bytes;
			_length = len;
		}
		else {
			char *buf = (char*)x_malloc(len + 1);
			memcpy(buf, bytes, len);
			// add NULL that's not included in length so that _str can be
			// directly used as a C string (which assumes that the string
			// does not contain NULLs of course)
			buf[len] = 0; 
			_str = buf;
			_length = len; // does NOT include NULL
		}
	}

	int length() const { return _length; }
	const char *as_cstr() const { return _str; }

	protected:
	const char *_str;
	int _length;
	// NOTE: size of str is length+1 to add a null at the end
};

// make a string type that can hold binary data
typedef std::vector<char> STRINGTYPE;

// this is intended to be a POD type, so no non-default constructors, destructors, base classes,
// and small enough to pass as value -- any large parts will be stored in pointers
#if defined(USE_GCMALLOC)
// note -- adding 'public gc' here, in contradiction of the above ... not sure if
// this is necessary or not
class Object : public gc {
#else
class Object {
#endif	
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
	// 'version 2' closures
	bool isBoundLambda() const { return type == TYPE_BOUND_LAMBDA; }
	bool isOpcode() const { return type == TYPE_OPCODE; }

	// convenience -- test if symbol AND equal to given string
	// if n>0 then only require that many chars to match
	bool isSymbol(const char *s, int n=0) const;
	bool isList() const { return type == TYPE_LIST; }
	bool isDict() const { return type == TYPE_DICT; }
			
	// get value (make sure to check first)
	VINT asInt() const { return data.i; }
	bool asBool() const { return data.b; }
	ObjList* asLambda() const { return data.objlist; };
	double asFloat() const { return data.d; }
	ObjList *asList() const { return data.objlist; }
	ObjDict *asDict() const { return data.objdict; }
	 
	const StringData *asString() const { return data.str; }
	const StringData *asSymbol() const { return data.str; }
	
	uint64_t asOpcode() const { return data.opcode; }

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
		// use vector instead of std::string so it can hold any binary data
		StringData *str; // strings & symbols, immutable
		BoundLambda *boundLambda;
		int64_t opcode;
	} data;
};

// for simplicity all call frames are the same size, so this may
// need some tuning so they aren't too large but large enough
// for all practical cases. for one thing, this avoids an extra
// allocation when creating a CallFrameData
//
// this is the maximum number args + locals a function can have
//
// *** WHEN CHANGING THIS, MUST CHANGE IT IN ALL PORTS AS WELL ***
const int MAX_CALLFRAME_SLOTS = 32;

// 'version 2' closures based on persistent frames
// every time a userword is called (or lambda via 'call')
// a new frame data is created for it.
#if defined(USE_GCMALLOC)
class CallFrameData : public gc_cleanup {
#else
class CallFrameData {
#endif
	public:
	CallFrameData();
	~CallFrameData() 
		{ outer = NULL; bound = false; }

	// frames begin disconnected from any other context. this
	// can be used to tie this frame to an outer context so that
	// getOuterObj/setOuterObj can be used (can also pass NULL to
	// disconnect this frame from any outer context)
	void setOuterFrame(CallFrameData *outer);

	// get/set object in frame up #levels (0 == this frame)
	Object getFrameObj(int levels, int index);
	void setFrameObj(int levels, int index, Object obj);

	bool isBound() const { return bound; }
	void setBound(bool b) { bound = b; }

	void clear_data();
	
	private:
	Object data[MAX_CALLFRAME_SLOTS]; // args+locals for function
	CallFrameData *outer;
	bool bound; // has this been bound to a lambda as its outer frame

	CallFrameData *findFrameUp(int levels);
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
// copies s
Object newString(const char *s, size_t len);
Object newString(const std::string& );
// takes existing StringData instead of copying
Object newString(StringData *data);
// like above
Object newSymbol(const char *s, size_t len);
Object newSymbol(const std::string& );
// takes existing StringData instead of copying
Object newSymbol(StringData *data);

Object newList(); // always makes empty list
Object newList(ObjList *); // wraps existing list, does NOT copy

Object newDict(); // make an empty dict

// 'version 2' closures
// NOTE: *my* frame is not known until bound-lambda is called.
// however, outerFrame is set when bind-lambda is called.
Object newBoundLambda(Object lambda, CallFrameData *outerFrame);

Object newOpcode(uint64_t packed_opcode);

// to deepcopy just the ObjList portion of a Object
ObjList *deepcopy(ObjList *objlist);