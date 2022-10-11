/*
	Data types / object system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#ifndef __langtypes_h__
#define __langtypes_h__

#define TYPE_NULL 0
#define TYPE_INT 1
#define TYPE_BOOL 2
#define TYPE_LAMBDA 3
#define TYPE_FLOAT 4
#define TYPE_STRING 5
#define TYPE_SYMBOL 6
#define TYPE_LIST 7
// a void type which is differentiated from null.
// in general, void is used in eof-type situations but can be used in any context
// where a function needs to differentiate a return value of null from a
// return of 'nothing'. for example the verbii compiler needs to be able to do this
// so that null can be a literal. void can never be a literal since the same problem
// would recur that there would be no way to differentiate a parsed void from eof.
// void will only ever be a word, so it will always be parsed as a symbol. since void
// normally shouldn't be used in data anyways, i don't think that's a problem.
#define TYPE_VOID 8
#define TYPE_DICT 9
// 'version 2' closure
#define  TYPE_BOUND_LAMBDA 10
// interpreter opcodes
#define  TYPE_OPCODE 11
// for internal use only - not visible from verbii - this is for convenience so
// that a void (f*)() can be stored in an Object to be put in a dict.
#define TYPE_VOIDFUNCPTR 12
// internal use, not visible from verbii
#define TYPE_CALLFRAMEDATA 13
#define TYPE_LAST_PLUS_1 14

// map above types to names
extern const char *TYPE_TO_NAME[TYPE_LAST_PLUS_1];
// stats on # allocs of each type
unsigned long int ALLOCS_BY_TYPE[TYPE_LAST_PLUS_1];
// stats on # deallocs of each type
unsigned long int DEALLOCS_BY_TYPE[TYPE_LAST_PLUS_1];

#include <stdint.h> // int64_t
#include "xmalloc.h"

// verbii's integer type
typedef int64_t VINT;

#define TRUE 1
#define FALSE 0

extern int FLOAT_PRECISION;
extern unsigned long int NR_SMALL_INT_ALLOCS;

#include "../../extern/c/uthash.h"

struct _ObjDictEntry;
struct _CallFrameData;
struct _Lambda;
struct _ObjArray;
struct _StringBuffer;

typedef void (*VoidFunctionPtr)();

typedef struct _Object {
	unsigned char type;
	union {
		VINT i; // ints, bools (TRUE|FALSE)
		struct _ObjArray *array; // for lists
		struct _ObjDictEntry *objdict; // for dict
		double d;
		struct _StringBuffer *buffer; // strings & symbols
		struct _Lambda *lambda; // bound & unbound lambdas
		// TODO -- why can't this just be in .i??
		uint64_t opcode;
		// so that a void (*fn)() can be wrapped - for internal use, not visible from verbii
		VoidFunctionPtr funcptr;
		struct _CallFrameData *framedata;
	} data;

	#if defined(USE_GC_OBJECT)
	// for garbage collector
	uint8_t gc_mark;
	uint8_t gc_count;
	struct _Object *gc_next;
	#endif
} Object;

// sync with C++ port
#define MAX_CALLFRAME_SLOTS 32

// *** TODO *** make this an Object so it can work with the gc_object system
typedef struct _CallFrameData {
	Object *data[MAX_CALLFRAME_SLOTS];
	Object *outer; // outer frame or NULL if top level
	int bound;
} CallFrameData;

Object* new_CallFrameData();
// get/set object in frame up #levels (0 == this frame)
Object* callframe_GetFrameObj(Object *frame, int levels, int index);
void callframe_SetFrameObj(Object *frame, int levels, int index, Object *obj);
void callframe_clear(CallFrameData *framedata);
int callframe_isBound(Object *frame);
void callframe_setBound(Object *frame, int bound);
Object *callframe_getOuter(Object *frame);
void callframe_setOuter(Object *frame, Object *outer);

typedef struct _ObjDictEntry {
	const char *name;
	struct _Object *obj;
	UT_hash_handle hh;
} ObjDictEntry;

// for both bound & unbound lambdas
typedef struct _Lambda {
	Object *list;
	Object *outer; // outer CallFrameData or NULL if not found
} Lambda;

typedef struct _ObjArray {
	Object **items;
	int length, maxsize;
} ObjArray;

#define min(a,b) ((a<b) ? (a) : (b))
#define max(a,b) ((a>b) ? (a) : (b))

// call this before using any other functions here
void init_langtypes();

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

// require obj be given type or error
void requiretype(const char *where, Object *obj, int type);

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

// for efficiency, strings & symbols are built on a StringBuffer which
// is writeable; however at the verbii level, strings & symbols are immutable.
typedef struct _StringBuffer {
	char *ptr;
	int maxsize;
	// space used INCLUDING added null at end so ptr can be used directly as C-string
	// (so true length of content is used-1)
	int used; 
} StringBuffer;

Object* newString(const char *s, int len); // if len<0, use strlen(s)
Object* newSymbol(const char *s, int len); // if len<0, use strlen(s)

// the following functions work on the internal StringBuffer, so will
// work on both symbols & strings, despite being named string_*

// append 1 character to string
void string_addchar(Object *string, char c);
// get length (since strings can store non NULL-terminated strings)
int string_length(Object *string);
// get as NULL-terminated string - this only works on non-binary data
// that does not contain any other NULLs
// NOTE: returned pointer must be used immediately (typically in a printf()),
// before returning to the interpreter loop, or strdup'd if needed for long 
// term use, since it is subject to garbage collection
const char *string_cstr(Object *string);
// append text to string in printf() way
void string_printf(Object *string, const char *fmt, ...);
// append another string to this one (this is more efficient that doing
// a string_printf(s, "%s", ...) and works with binary data as well
void string_append(Object *string, Object *other);
// ... or to append a C string; pass nr=-1 to use strlen(other)
void string_append_cstr(Object *string, const char *other, int nr);

Object* newLambda(Object *list); // starts unbound
// pass the LIST not another lambda
// if framedata != NULL, sets .bound to TRUE
Object* newBoundLambda(Object *list, Object *framedata);

// the lower level array object
ObjArray *newObjArray();
void ObjArray_append(ObjArray* list, Object *obj);
int ObjArray_length(ObjArray* list);
Object* ObjArray_get(ObjArray* list, int i);
void ObjArray_put(ObjArray* list, int i, Object *obj);

// the verbii list object
Object* newList(); // always makes empty list
// extended list creation:
//	initsize - set initial capacity to initsize (but still empty)
//	fill - if != NULL, fill initval elements with this object so length==initsize
Object* newListEx(int initsize, Object *fill);
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

void langtypes_print_stats();

#if defined(USE_GC_OBJECT)
// ONLY to be called by gc -- marks objects known by langtypes.c 
void langtypes_mark_reachable_objects();

// ONLY to be called by gc -- frees extra memory associate with object (NOT object itself)
void freeobj_string(Object *str);
void freeobj_symbol(Object *str);
void freeobj_list(Object *list);
void freeobj_dict(Object *dict);
void freeobj_lambda(Object *lambda);
void freeobj_callframedata(Object *frame);

#endif

#endif // __langtypes_h__