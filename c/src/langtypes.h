/*
	Data types / object system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#ifndef __langtypes_h__
#define __langtypes_h__

#define TYPE_NULL 0
#define  TYPE_INT 1
#define  TYPE_BOOL 2
#define  TYPE_LAMBDA 3
#define  TYPE_FLOAT 4
#define  TYPE_STRING 5
#define  TYPE_SYMBOL 6
#define  TYPE_LIST 7
// a void type which is differentiated from null.
// in general, void is used in eof-type situations but can be used in any context
// where a function needs to differentiate a return value of null from a
// return of 'nothing'. for example the verbii compiler needs to be able to do this
// so that null can be a literal. void can never be a literal since the same problem
// would recur that there would be no way to differentiate a parsed void from eof.
// void will only ever be a word, so it will always be parsed as a symbol. since void
// normally shouldn't be used in data anyways, i don't think that's a problem.
#define  TYPE_VOID 8
#define TYPE_DICT 9
// 'version 2' closure
#define  TYPE_BOUND_LAMBDA 10
// interpreter opcodes
#define  TYPE_OPCODE 11
// for internal use only - not visible from verbii - this is for convenience so
// that a void (f*)() can be stored in an Object to be put in a dict.
#define TYPE_VOIDFUNCPTR 12

#include <stdint.h> // int64_t
#include "xmalloc.h"

// verbii's integer type
typedef int64_t VINT;

#define TRUE 1
#define FALSE 0

extern int FLOAT_PRECISION;

#include "utstring.h"
#include "uthash.h"

struct _ObjDictEntry;
struct _CallFrameData;
struct _Lambda;
struct _ObjArray;

typedef void (*VoidFunctionPtr)();

typedef struct _Object {
	unsigned char type;
	union {
		VINT i; // ints, bools (TRUE|FALSE)
		struct _ObjArray *array; // for lists
		struct _ObjDictEntry *objdict; // for dict
		double d;
		UT_string *str; // strings & symbols
		struct _Lambda *lambda; // bound & unbound lambdas
		// TODO -- why can't this just be in .i??
		uint64_t opcode;
		// so that a void (*fn)() can be wrapped - for internal use, not visible from verbii
		VoidFunctionPtr funcptr;
	} data;
} Object;

// sync with C++ port
#define MAX_CALLFRAME_SLOTS 32

typedef struct _CallFrameData {
	Object *data[MAX_CALLFRAME_SLOTS];
	struct _CallFrameData *outer;
	int bound;
} CallFrameData;

CallFrameData* new_CallFrameData();
// get/set object in frame up #levels (0 == this frame)
Object* callframe_GetFrameObj(CallFrameData *frame, int levels, int index);
void callframe_SetFrameObj(CallFrameData *frame, int levels, int index, Object *obj);
void callframe_clear(CallFrameData *frame);

typedef struct _ObjDictEntry {
	const char *name;
	struct _Object *obj;
	UT_hash_handle hh;
} ObjDictEntry;

// for both bound & unbound lambdas
typedef struct _Lambda {
	Object *list;
	CallFrameData *outer;
} Lambda;

typedef struct _ObjArray {
	Object **items;
	int length, maxsize;
} ObjArray;

#define min(a,b) ((a<b) ? (a) : (b))
#define max(a,b) ((a>b) ? (a) : (b))

// call this before using any other functions here
void init_object_system();

int isNull(Object *obj);
int isVoid(Object *obj);
int isInt(Object *obj);
int isFloat(Object *obj);
int isBool(Object *obj);
int isString(Object *obj);
int isSymbol(Object *obj);
// test that obj is a symbol AND either a partial or full match to name
// if nr_match < 0 then tests full length of name
int isSymbolMatch(Object *obj, const char *name, int nr_match);
int isLambda(Object *obj);
int isBoundLambda(Object *obj);
int isList(Object *obj);
int isDict(Object *obj);
int isOpcode(Object *obj);
int isVoidFunctionPtr(Object *obj);

Object* newNull();
Object* newVoid();
Object* newInt(VINT i);
Object* newFloat(double d);

Object* parseInt(const char *str);
Object* parseFloat(const char *str);
Object* parseBool(const char *str);

int isNumber(Object *a);
double asNumber(Object *a); // convert int or float to double

Object* newBool(VINT b);

Object* newString(const char *s, int len); // if len<0, use strlen(s)
Object* newSymbol(const char *s, int len); // if len<0, use strlen(s)

// works for strings OR symbols
int string_length(Object *s);
// get as NULL-terminated string - works for strings OR symbols
const char *string_cstr(Object *s);

Object* newLambda(Object *list); // starts unbound
// pass the LIST not another lambda
Object* newBoundLambda(Object *list, CallFrameData *data);

// the lower level array object
ObjArray *newObjArray();
void ObjArray_append(ObjArray* list, Object *obj);
int ObjArray_length(ObjArray* list);
Object* ObjArray_get(ObjArray* list, int i);
void ObjArray_put(ObjArray* list, int i, Object *obj);

// the verbii list object
Object* newList(); // always makes empty list
Object* newListKeepArray(ObjArray *array); // keeps pointer
void List_append(Object* list, Object *obj);
int List_length(Object* list);
Object* List_get(Object* list, int i);
void List_put(Object* list, int i, Object *obj);

Object* newDict(); // makes empty dict
void Dict_put(Object *dict, const char *key, Object *obj);
Object* Dict_get(Object *dict, const char *key);
void Dict_delete(Object *dict, const char *key);
int Dict_size(Object *dict);

Object* newOpcode(uint64_t packed_opcode);

// internal use only - not visible from verbii
Object *newVoidFunctionPtr(VoidFunctionPtr funcptr);

// generic function for strings/symbols/lists/dicts
int length(Object *obj);

Object *deepcopy(Object *obj);
int testGreater(Object *a, Object *b);
int testEqual(Object *a, Object *b);

const char* fmtDisplayPrint(Object *obj);
const char* fmtStackPrint(Object *obj);

#endif // __langtypes_h__