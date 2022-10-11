/*
	Data types / object system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "langtypes.h"
#include "errors.h"
#include <assert.h>
#include <stdlib.h>
#include "opcodes.h"
#include "gc_object.h"

// singletons

Object *THE_NULL = NULL;
Object *THE_VOID = NULL;
Object *THE_TRUE = NULL;
Object *THE_FALSE = NULL;

int FLOAT_PRECISION = 17;

const char *TYPE_TO_NAME[] = { "null", "int", "bool", "lambda", "float", "string", "symbol",
		"list", "void", "dict", "bound-lambda", "opcode", "void-funcptr", "callframe-data" };
		
unsigned long int ALLOCS_BY_TYPE[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0};
unsigned long int DEALLOCS_BY_TYPE[] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0};

unsigned long int NR_SMALL_INT_ALLOCS = 0;

void init_object_system() {
	THE_NULL = new_gc_object(TYPE_NULL);
	THE_VOID = new_gc_object(TYPE_VOID);
	THE_TRUE = new_gc_object(TYPE_BOOL);
	THE_TRUE->data.i = TRUE;
	THE_FALSE = new_gc_object(TYPE_BOOL);
	THE_FALSE->data.i = FALSE;
}

void langtypes_mark_reachable_objects() {
	// mark my objects that otherwise would not be found by gc
	gc_mark_object(THE_NULL);
	gc_mark_object(THE_VOID);
	gc_mark_object(THE_TRUE);
	gc_mark_object(THE_FALSE);	
}

int isNull(Object *obj) { return (obj->type == TYPE_NULL) ? TRUE : FALSE; }
int isVoid(Object *obj) { return (obj->type == TYPE_VOID) ? TRUE : FALSE; }
int isInt(Object *obj) { return (obj->type == TYPE_INT) ? TRUE : FALSE; }
int isFloat(Object *obj) { return (obj->type == TYPE_FLOAT) ? TRUE : FALSE; }
int isBool(Object *obj) { return (obj->type == TYPE_BOOL) ? TRUE : FALSE; }
int isString(Object *obj) { return (obj->type == TYPE_STRING) ? TRUE : FALSE; }
int isSymbol(Object *obj) { return (obj->type == TYPE_SYMBOL) ? TRUE : FALSE; }
// lambda & bound-lambda are distinct types, even though they are implemented with a 
// single object at the C level
int isLambda(Object *obj) { return (obj->type == TYPE_LAMBDA) ? TRUE : FALSE; }
int isBoundLambda(Object *obj) { return (obj->type == TYPE_BOUND_LAMBDA) ? TRUE : FALSE; }
int isList(Object *obj) { return (obj->type == TYPE_LIST) ? TRUE : FALSE; }
int isDict(Object *obj) { return (obj->type == TYPE_DICT) ? TRUE : FALSE; }
int isOpcode(Object *obj) { return (obj->type == TYPE_OPCODE) ? TRUE : FALSE; }
int isVoidFunctionPtr(Object *obj) { return (obj->type == TYPE_VOIDFUNCPTR) ? TRUE : FALSE; }

void requiretype(const char *where, Object *obj, int type) {
	if(obj->type != type)
		error("%s expects %s but got %s", where, TYPE_TO_NAME[type], fmtStackPrint(obj));
}

void require_stringlike(const char *where, Object *obj) {
	if(obj->type != TYPE_STRING && obj->type != TYPE_SYMBOL)
		error("%s expects string or symbol but got %s", where, fmtStackPrint(obj));
}

Object* newInt(VINT i) {
	// adjust threshold experimentally to find majority use case
	if(i >= -10 && i <= 1000)
		++NR_SMALL_INT_ALLOCS;

	Object *obj = new_gc_object(TYPE_INT);
	obj->data.i = i;
	return obj;
}

Object* newNull() {
	return THE_NULL;
}

Object* newVoid() {
	return THE_VOID;
}

Object* newBool(VINT b) {
	return b == 0? THE_FALSE : THE_TRUE;	
}

Object* newFloat(double d) {
	Object *obj = new_gc_object(TYPE_FLOAT);
	obj->data.d = d;
	return obj;
}

Object* parseInt(const char *str) {
	// parser validates input format, so this should always succeed
	return newInt(atoll(str));
}

Object* parseFloat(const char *str) {
	// as above, str should have been validated by parser
	return newFloat(atof(str));
}

Object* parseBool(const char *str) {
	if(!strcmp(str,"true"))
		return newBool(TRUE);
	else if(!strcmp(str,"false"))
		return newBool(FALSE);
	else
		error("Bad boolean literal: %s", str);
}

// minimum number of chars to expand buffer by at a time
#define STRINGBUF_MIN_ALLOC 16

// length is zero but can hold initsize chars (plus internally-added NULL)
static StringBuffer *new_string_buffer(int initsize) {
	StringBuffer *buf = (StringBuffer*)x_malloc(sizeof(StringBuffer));
	buf->maxsize = max(initsize+1, STRINGBUF_MIN_ALLOC);
	buf->ptr = (char*)x_malloc(buf->maxsize);
	buf->ptr[0] = 0;
	buf->used = 1; // empty string + NULL
	return buf;
}

static void free_string_buffer(StringBuffer *buf) {
	x_free(buf->ptr);
	x_free(buf);
}

// ensure buffer can hold an ADDITIONAL nr chars
static void stringbuf_ensure_space(StringBuffer *buf, int nr) {
	if((buf->maxsize - buf->used) < nr) {
		int needed = nr - (buf->maxsize - buf->used);
		int newsize = buf->maxsize + max(needed, STRINGBUF_MIN_ALLOC);
		buf->ptr = (char*)x_realloc(buf->ptr, newsize);
		buf->maxsize = newsize;
	}
}

// append 1 character to string
void string_addchar(Object *string, char c) {
	require_stringlike("string_addchar", string);
	StringBuffer *buf = string->data.buffer;
	stringbuf_ensure_space(buf, 1);
	buf->ptr[buf->used-1] = c;
	buf->ptr[buf->used] = 0; // always NULL terminate so cstr() can be used anytime
	++buf->used;
}

int string_length(Object *string) {
	require_stringlike("string_length", string);
	return string->data.buffer->used - 1;
}

const char *string_cstr(Object *string) {
	require_stringlike("string_cstr", string);
	// is always NULL terminated
	return string->data.buffer->ptr;
}

static void stringbuf_copy_in(StringBuffer *buffer, const char *src, int nrbytes) {
	stringbuf_ensure_space(buffer, nrbytes);
	memcpy(buffer->ptr+buffer->used-1, src, nrbytes);
	buffer->used += nrbytes;
	buffer->ptr[buffer->used-1] = 0; // always add \0
}

void string_append(Object *string, Object *other) {
	require_stringlike("string_append", string);
	stringbuf_copy_in(string->data.buffer, string_cstr(other), string_length(other));
}

void string_append_cstr(Object *string, const char *other, int nrbytes) {
	require_stringlike("string_append", string);
	if(nrbytes<0)
		nrbytes = strlen(other);

	stringbuf_copy_in(string->data.buffer, other, nrbytes);
}

#include <stdio.h>
#include <stdarg.h>

void string_printf(Object *string, const char *fmt, ...) {
	require_stringlike("string_printf", string);
	// first, determine how long string will be, using a temp buffer
	char temp[2];
	va_list args;
	va_start(args,fmt);
	int nr = vsnprintf(temp, 1, fmt, args);
	va_end(args);

	// now create actual buffer -- per C standard, nr is the number of chars
	// that would have been written NOT including the \0, so need +1
	char *buf = (char*)x_malloc(nr+1);
	va_start(args,fmt);
	// per C standard, this will write (nr-1) chars, then \0, so pass nr+1
	vsnprintf(buf, nr+1, fmt, args);
	va_end(args);

	// now copy into string
	stringbuf_copy_in(string->data.buffer, buf, nr);
	// free temp
	x_free(buf);
}

Object* newString(const char *s, int len) {
	Object *obj = new_gc_object(TYPE_STRING);
	if(len<0)
		len = strlen(s);

	obj->data.buffer = new_string_buffer(len);
	stringbuf_copy_in(obj->data.buffer, s, len);
	return obj;
}

void freeobj_string(Object *str) {
	require_stringlike("freeobj_string", str);
	free_string_buffer(str->data.buffer);
}

Object* newSymbol(const char *s, int len) {
	Object *obj = new_gc_object(TYPE_SYMBOL);
	if(len<0)
		len = strlen(s);

	obj->data.buffer = new_string_buffer(len);
	stringbuf_copy_in(obj->data.buffer, s, len);
	return obj;
}

void freeobj_symbol(Object *str) {
	free_string_buffer(str->data.buffer);
}

Object* newLambda(Object *list) {
	requiretype("newLambda", list, TYPE_LIST);
	Object *obj = new_gc_object(TYPE_LAMBDA);
	obj->data.lambda = (Lambda*)x_malloc(sizeof(Lambda));
	obj->data.lambda->list = list;
	obj->data.lambda->outer = NULL;
	return obj;
}

void freeobj_lambda(Object *lambda) {
	x_free(lambda->data.lambda);
}

Object *newVoidFunctionPtr(VoidFunctionPtr funcptr) {
	Object *obj = new_gc_object(TYPE_VOIDFUNCPTR);
	obj->data.funcptr = funcptr;
	return obj;
}

Object* newBoundLambda(Object *list, Object *framedata) {
	requiretype("newBoundLambda", list, TYPE_LIST);
	//printf("NEW BOUND LAMBDA FROM: %s @ %llx\n", fmtStackPrint(list), (long long unsigned int)list);
	Object *obj = new_gc_object(TYPE_BOUND_LAMBDA);
	obj->data.lambda = (Lambda*)x_malloc(sizeof(Lambda));
	obj->data.lambda->list = list;
	obj->data.lambda->outer = framedata;
	callframe_setBound(framedata, TRUE);
	//printf("MADE OBJECT: %s\n", fmtStackPrint(obj));
	return obj;
}

Object* newOpcode(uint64_t packed_opcode) {
	Object *obj = new_gc_object(TYPE_OPCODE);
	obj->data.opcode = packed_opcode;
	return obj;
}

int isSymbolMatch(Object *obj, const char *name, int nr_match) {
	if(!isSymbol(obj))
		return 0;

	if(nr_match < 0) 
		return !strcmp(name, string_cstr(obj));
	else
		return !strncmp(name, string_cstr(obj), nr_match);
}

int isNumber(Object *a) {
	return (a->type == TYPE_INT || a->type == TYPE_FLOAT) ? TRUE : FALSE;
}

double asNumber(Object *a) {
	switch(a->type) {
		case TYPE_INT: return a->data.i;
		case TYPE_FLOAT: return a->data.d;
		default: error("asNumber() expects int or float, got: %s", fmtStackPrint(a));
	}
}

int length(Object *obj) {
	switch(obj->type) {
		case TYPE_STRING:
		case TYPE_SYMBOL:
			return string_length(obj);
		case TYPE_LIST:
			return List_length(obj);
		case TYPE_DICT:
			return Dict_size(obj);
	}
	error("'length' not supported for object: %s", fmtStackPrint(obj));
}

Object* newListEx(int initsize, Object *fill) {
	Object *obj = new_gc_object(TYPE_LIST);
	
	ObjArray *array = (ObjArray*)x_malloc(sizeof(ObjArray));
	array->maxsize = (initsize > 0) ? initsize : 10;
	array->items = (Object**)x_malloc(array->maxsize * sizeof(Object*));
	array->length = 0;

	if(initsize > 0 && fill != NULL) {
		for(int i=0; i<initsize; ++i)
			array->items[i] = fill;

		array->length = initsize;
	}

	obj->data.array = array;
	return obj;
}

Object *newList() {
	// start with a small nonzero maxsize empty list
	return newListEx(10, NULL);
}

void freeobj_list(Object *list) {
	x_free(list->data.array->items);
	x_free(list->data.array);
}

void List_append(Object *list, Object *obj) {
	requiretype("append", list, TYPE_LIST);
	if(list->data.array->length == list->data.array->maxsize) {
		// grow by 10% but at least by 10
		int newsize = list->data.array->maxsize + max((int)(list->data.array->maxsize*0.1), 10);
		Object **newptr = (Object**)x_realloc(list->data.array->items, newsize*sizeof(Object*));
		if(!newptr)
			error("Out of memory!");

		list->data.array->items = newptr;
		list->data.array->maxsize = newsize;
	}
	list->data.array->items[list->data.array->length++] = obj;
}

int List_length(Object *list) {
	requiretype("length", list, TYPE_LIST);
	return list->data.array->length;
}

Object* List_get(Object *list, int i) {
	requiretype("get", list, TYPE_LIST);
	if(i < 0 || i >= list->data.array->length)
		return newVoid(); // index out of range is OK - returns void
	else
		return list->data.array->items[i];
}

void List_put(Object *list, int i, Object *obj) {
	requiretype("put", list, TYPE_LIST);
	if(i < 0 || i >= list->data.array->length)
		// out of bounds NOT allowed on put()
		error("List index out of bounds in put(): %d", i);
	
	list->data.array->items[i] = obj;
}

Object *newDict() {
	Object *obj = new_gc_object(TYPE_DICT);
	obj->data.objdict = NULL;
	return obj;
}

void Dict_put(Object *dict, const char *key, Object *obj) {
	ObjDictEntry *ent = NULL;
	HASH_FIND(hh, dict->data.objdict, key, strlen(key), ent);
	if(!ent) {
		ObjDictEntry *ent = (ObjDictEntry*)x_malloc(sizeof(ObjDictEntry));
		// copy key to (1) ensure it doesn't go away and (2) ensure it is immutable
		ent->name = x_strndup(key, strlen(key));
		ent->obj = obj;
		HASH_ADD_KEYPTR(hh, dict->data.objdict, ent->name, strlen(ent->name), ent);
	}
	else
		ent->obj = obj;
}

void freeobj_dict(Object *dict) {
	#if 0
	ObjDictEntry *ent = dict->data.objdict;
	while(ent) {
		ObjDictEntry *to_free = ent;
		ent = ent->hh.next;
		x_free(to_free);
	}
	#endif
	// the right way from: https://troydhanson.github.io/uthash/userguide.html
	ObjDictEntry *ent, *tmp;
	HASH_ITER(hh, dict->data.objdict, ent, tmp) {
		HASH_DEL(dict->data.objdict, ent);
		x_free(ent);
	}
}

Object *Dict_get(Object *dict, const char *key) {
	ObjDictEntry *ent = NULL;
	HASH_FIND(hh, dict->data.objdict, key, strlen(key), ent);
	if(!ent)
		return THE_VOID;
	else
		return ent->obj;
}

void Dict_delete(Object *dict, const char *key) {
	ObjDictEntry *ent = NULL;
	HASH_FIND(hh, dict->data.objdict, key, strlen(key), ent);
	if(ent)
		HASH_DEL(dict->data.objdict, ent);
}

int Dict_size(Object *dict) {
	return HASH_COUNT(dict->data.objdict);
}

int sort_objdictentry_by_name(ObjDictEntry *a, ObjDictEntry *b) {
	return strcmp(a->name, b->name);
}

static Object *findFrameUp(Object *frame, int levels) {
	requiretype("findFrameUp", frame, TYPE_CALLFRAMEDATA);
	while(levels > 0) {
		if(!frame || !frame->data.framedata->outer)
			error("Bad level number in findFrameUp()");

		levels -= 1;
		frame = frame->data.framedata->outer;
	}
	return frame; // cannot be NULL due to above checks
}

Object* new_CallFrameData() {
	Object *obj = new_gc_object(TYPE_CALLFRAMEDATA);

	CallFrameData* cf = (CallFrameData*)x_malloc(sizeof(CallFrameData));
	//memset(cf->data, 0, MAX_CALLFRAME_SLOTS*sizeof(Object*));
	callframe_clear(cf);
	cf->outer = NULL;
	cf->bound = 0;

	obj->data.framedata = cf;
	return obj;
}

int callframe_isBound(Object *frame) {
	requiretype("callframe_isBound", frame, TYPE_CALLFRAMEDATA);
	return frame->data.framedata->bound;
}

void callframe_setBound(Object *frame, int bound) {
	requiretype("callframe_setBound", frame, TYPE_CALLFRAMEDATA);
	frame->data.framedata->bound = bound;
}

Object *callframe_getOuter(Object *frame) {
	requiretype("callframe_getOuter", frame, TYPE_CALLFRAMEDATA);
	return frame->data.framedata->outer;
}

void callframe_setOuter(Object *frame, Object *outer) {
	requiretype("callframe_setOuter", frame, TYPE_CALLFRAMEDATA);
	frame->data.framedata->outer = outer;
}

void freeobj_callframedata(Object *frame) {
	requiretype("freeobj_callframedata", frame, TYPE_CALLFRAMEDATA);
	x_free(frame->data.framedata);
}

Object* callframe_GetFrameObj(Object *frame, int levels, int index) {
	requiretype("findFrameUp", frame, TYPE_CALLFRAMEDATA);
	if(index < 0 || index >= MAX_CALLFRAME_SLOTS)
		error("Out of bounds in callframe_GetFrameObj()");
	// go up number of levels
	frame = findFrameUp(frame, levels);
	return frame->data.framedata->data[index];
}

void callframe_SetFrameObj(Object *frame, int levels, int index, Object *obj) {
	requiretype("findFrameUp", frame, TYPE_CALLFRAMEDATA);
	if(index < 0 || index >= MAX_CALLFRAME_SLOTS)
		error("Out of bounds in callframe_SetFrameObj()");
	// go up number of levels
	frame = findFrameUp(frame, levels);
	frame->data.framedata->data[index] = obj;
}

void callframe_clear(CallFrameData *frame) {
	for(int i=0; i<MAX_CALLFRAME_SLOTS; ++i)
		frame->data[i] = THE_NULL;
	//memset(frame->data, 0, MAX_CALLFRAME_SLOTS*sizeof(CallFrameData*));

	frame->bound = 0;
	frame->outer = NULL;
}

int testEqual(Object *a, Object *b) {
	switch(a->type) {
		case TYPE_NULL: return isNull(b);
		case TYPE_INT: return (b->type == TYPE_INT && b->data.i == a->data.i) ||
							(b->type == TYPE_FLOAT && b->data.d == a->data.i);
		case TYPE_FLOAT: return (b->type == TYPE_FLOAT && b->data.d == a->data.d) ||
							(b->type == TYPE_INT && b->data.i == a->data.d);
		case TYPE_BOOL: return b->type == TYPE_BOOL && b->data.i == a->data.i;
		case TYPE_LAMBDA: return FALSE; // lambdas never equal any other object, even themselves
		case TYPE_BOUND_LAMBDA: return FALSE; // same
		case TYPE_OPCODE: return (b->type == TYPE_OPCODE) &&
				(b->data.opcode == a->data.opcode);
		case TYPE_STRING: 
			return b->type == TYPE_STRING && 
					string_length(b) == string_length(a) &&
					!memcmp(string_cstr(a), string_cstr(b), string_length(a));
		case TYPE_SYMBOL: 
			return b->type == TYPE_SYMBOL && 
					string_length(b) == string_length(a) &&
					!memcmp(string_cstr(a), string_cstr(b), string_length(a));
		case TYPE_VOID: return b->type == TYPE_VOID;
		// lists are deep compared, with testEqual at each element
		case TYPE_LIST:	{
			if(b->type != TYPE_LIST)
				return FALSE;

			if(List_length(a) != List_length(b))
				return FALSE;

			for(int i=0; i<List_length(a); ++i) {
				if(!testEqual(List_get(a, i), List_get(b, i)))
					return FALSE;
			}
			return TRUE;
		}
		// dicts are deep compared
		case TYPE_DICT: {
			if(b->type != TYPE_DICT)
				return FALSE;

			if(Dict_size(a) != Dict_size(b))
				return FALSE;

			for(ObjDictEntry *ent=a->data.objdict; ent != NULL; ent = ent->hh.next) {
				ObjDictEntry *found;
				HASH_FIND_STR(b->data.objdict, ent->name, found);
				if(!found)
					return FALSE;

				if(!testEqual(ent->obj, found->obj))
					return FALSE;
			}

			return TRUE;
		}

		default: 
			// i WANT to crash when I forget to add a new type ...
			error("Unsupported type in == : %s", fmtStackPrint(a));
	}
}

int testGreater(Object *a, Object *b) {
	switch(a->type) {
		case TYPE_INT:
			if (b->type == TYPE_INT) return a->data.i > b->data.i;
			if (b->type == TYPE_FLOAT) return a->data.i > b->data.d;
			break;
		case TYPE_FLOAT: 
			if (b->type == TYPE_INT) return a->data.d > b->data.i;
			if (b->type == TYPE_FLOAT) return a->data.d > b->data.d;
			break;
		case TYPE_STRING:
		case TYPE_SYMBOL:
			if(b->type == a->type) {
				int r = memcmp(string_cstr(a), string_cstr(b), min(string_length(a), string_length(b)));
				if(r == 0) {
					if(string_length(a) < string_length(b))
						return FALSE;
					else if(string_length(a) > string_length(b))
						return TRUE;
					else
						return FALSE;
				}
				else
					return r > 0;
			}
		case TYPE_LIST:
		{
			if(b->type != TYPE_LIST)
				break;

			// like a string test, but on elements of list
			// its an error for elements to not be of the same types

			// loop over min of lengths
			size_t nr = min(List_length(a), List_length(b));

			for(size_t i=0; i<nr; ++i) {
				if(testGreater(List_get(a,i), List_get(b,i))) {
					// found first element where self>other, so entire test is TRUE
					return TRUE;
				}
				else if(!testEqual(List_get(a,i),List_get(b,i))) {
					// !greater && !equal, so b[i] is less, so entire test is FALSE
					return FALSE;
				}
				// else a[i] == b[i]; continue with next element
			}
			// reaching this point means all are equal up to nr elements, so:
			// if self longer than other, self is greater
			if(List_length(a) > List_length(b))
				return TRUE;
			// else other is longer or equal, so self is not greater
			else
				return FALSE;	
		}
	}
	error("Cannot compare objects in >: %s and %s", fmtStackPrint(a), fmtStackPrint(b));
}

Object *deepcopy(Object *obj) {
	switch(obj->type) {
		// all atomic or read-only types just return themselves
		case TYPE_NULL:
		case TYPE_VOID:
		case TYPE_INT:
		case TYPE_FLOAT: 
		case TYPE_BOOL:
		case TYPE_LAMBDA:
		case TYPE_BOUND_LAMBDA:
		case TYPE_STRING:
		case TYPE_SYMBOL:
		case TYPE_OPCODE:
			return obj;

		case TYPE_LIST: {
			Object *newlist = newListEx(List_length(obj),NULL);
			newlist->data.array->length = List_length(obj);
			for(int i=0; i<List_length(obj); ++i)
				List_put(newlist, i, deepcopy(List_get(obj, i)));
			
			return newlist;
		}
		case TYPE_DICT: {
			Object *newdict = newDict();
			for(ObjDictEntry *ent=obj->data.objdict; ent != NULL; ent = ent->hh.next)
				Dict_put(newdict, ent->name, deepcopy(ent->obj));
			
			return newdict;
		}
	
		default: error("deepcopy not implemented for object type %d", obj->type);
	}
}

static const char* fmtDisplayPrintList(Object *list, const char* open_delim, const char* close_delim) {
	requiretype("fmtDisplayPrintList", list, TYPE_LIST);
	// use a String since it will be garbage collected automatically (only restriction is caller must
	// use returned char* before returning to interpreter)
	Object *str = newString("",0);
	string_printf(str, "%s ", open_delim);
	
	for(int i=0; i<List_length(list); ++i)
		string_printf(str, "%s ", fmtDisplayPrint(List_get(list,i)));	

	string_printf(str, "%s", close_delim);
	return string_cstr(str);
}

const char* fmtDisplayPrint(Object *obj) {
	Object *str = newString("",-1);
	switch(obj->type) {
		case TYPE_NULL: 
			return "<null>";
		case TYPE_VOID: 
			return "<*void*>";
		case TYPE_INT: 
			string_printf(str, "%ld", obj->data.i);
			return string_cstr(str);
		case TYPE_FLOAT: 
			string_printf(str, "%.*g", FLOAT_PRECISION, obj->data.d);
			return string_cstr(str);
		case TYPE_BOOL: 
			return (obj->data.i == TRUE) ? "true" : "false";
		case TYPE_LIST:
			return fmtDisplayPrintList(obj, "[", "]");
		case TYPE_LAMBDA:
			//printf("STACK PRINT LAMBDA @ %llx\n", (long long unsigned int)obj->data.lambda->list);
			if(obj->data.lambda->outer)
				error("Got LAMBDA with outer data instead of BOUND-LAMBDA");
			
			return fmtDisplayPrintList(obj->data.lambda->list, "{", "}");
		case TYPE_BOUND_LAMBDA:
			if(!obj->data.lambda->outer)
				error("Got BOUND-LAMBDA with NULL outer frame");

			return fmtDisplayPrintList(obj->data.lambda->list, "<bound {", "}>");	
		case TYPE_STRING:
		case TYPE_SYMBOL:
			{
				int i;
				for(i=0; i<string_length(obj); ++i) {
					char c = string_cstr(obj)[i];
					if(c < 32 || c > 126) {
						// turn non-printable chars into '%code'						
						string_printf(str, "\\x%02x", (int)((unsigned char)c));
					}
					else
						string_addchar(str, c);
				}
				return string_cstr(str);
			}
		case TYPE_OPCODE:
			return fmtStackPrint(obj);
		case TYPE_DICT: 
		{
			HASH_SORT(obj->data.objdict, sort_objdictentry_by_name);
			ObjDictEntry *ent;
			string_printf(str, "{ ");
			for(ent=obj->data.objdict; ent != NULL; ent = ent->hh.next) {
				string_printf(str, "\"%s\" => %s ", ent->name, fmtDisplayPrint(ent->obj));
			}
			string_printf(str,"}");
			return string_cstr(str);
		}

		default: 
			error("** UNKNOWN TYPE IN fmtDisplayPrint: %d\n", obj->type);
	}
}

static const char* fmtStackPrintList(Object *list, const char *open_delim, const char *close_delim) {
	requiretype("fmstStackPrintList", list, TYPE_LIST);
	// as above, use a String
	Object *str = newString("",0);
	string_printf(str, "%s ", open_delim);
	
	//printf("STACK PRINT ARRAY @ %llx\n", (long long unsigned int)arr);

	for(int i=0; i<List_length(list); ++i)
		string_printf(str, "%s ", fmtStackPrint(List_get(list,i)));

	string_printf(str, "%s", close_delim);
	return string_cstr(str);
}

const char* fmtStackPrint(Object *obj) {
	// as above, use a string
	Object *str = newString("",-1);
	switch(obj->type) {
		case TYPE_NULL: 
			return "<null>";
		case TYPE_VOID: 
			return "<*void*>";
		case TYPE_INT: 
			string_printf(str, "%ld", obj->data.i);
			return string_cstr(str);
		case TYPE_FLOAT:
			string_printf(str, "#%.*g", FLOAT_PRECISION, obj->data.d);
			return string_cstr(str);
		case TYPE_BOOL: 
			return (obj->data.i == TRUE) ? "<true>" : "<false>";
		case TYPE_LIST:
			return fmtStackPrintList(obj, "[", "]");
		case TYPE_LAMBDA:
			//printf("STACK PRINT LAMBDA @ %llx\n", (long long unsigned int)obj->data.lambda->list);
			if(obj->data.lambda->outer)
				error("Got LAMBDA with outer data instead of BOUND-LAMBDA");
			
			return fmtStackPrintList(obj->data.lambda->list, "{", "}");
		case TYPE_BOUND_LAMBDA:
			if(!obj->data.lambda->outer)
				error("Got BOUND-LAMBDA with NULL outer frame");

			return fmtStackPrintList(obj->data.lambda->list, "<bound {", "}>");			
		case TYPE_STRING:
			string_printf(str, "\"%s\"", fmtDisplayPrint(obj));
			return string_cstr(str);
		case TYPE_OPCODE:
		{
			uint8_t code;
			uint8_t A;
			uint16_t B;
			uint32_t C;
			opcode_unpack(obj->data.opcode, &code, &A, &B, &C);
			string_printf(str, "#op( %s %d %d %d )", opcode_code_to_name(code), (int)A, (int)B, (int)C);
			return string_cstr(str);
		}
		case TYPE_SYMBOL:
			string_printf(str, "'%s", fmtDisplayPrint(obj));
			return string_cstr(str);
		case TYPE_DICT: 
		{
			HASH_SORT(obj->data.objdict, sort_objdictentry_by_name);
			ObjDictEntry *ent;
			string_printf(str, "{ ");
			for(ent=obj->data.objdict; ent != NULL; ent = ent->hh.next) {
				string_printf(str, "\"%s\" => %s ", ent->name, fmtStackPrint(ent->obj));
			}
			string_printf(str,"}");
			return string_cstr(str);
		}
		case TYPE_VOIDFUNCPTR:
			string_printf(str, "<funcptr %llx>", (long long)obj->data.funcptr);
			return string_cstr(str);
		default: 
			error("** UNKNOWN TYPE IN fmtStackPrint: %d\n", obj->type);
	}
}