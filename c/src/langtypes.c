/*
	Data types / object system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "langtypes.h"
#include "errors.h"
#include <assert.h>
#include <stdlib.h>
#include "utstring.h"

// singletons

Object *THE_NULL = NULL;
Object *THE_VOID = NULL;
Object *THE_TRUE = NULL;
Object *THE_FALSE = NULL;


static Object *basic_object(unsigned char type) {
	Object *obj = (Object*)x_malloc(sizeof(Object));
	obj->type = type;
	return obj;
}

void init_object_system() {
	THE_NULL = basic_object(TYPE_NULL);
	THE_VOID = basic_object(TYPE_VOID);
	THE_TRUE = basic_object(TYPE_BOOL);
	THE_TRUE->data.i = 1;
	THE_FALSE = basic_object(TYPE_BOOL);
	THE_FALSE->data.i = 0;
}

Object* newInt(VINT i) {
	Object *obj = basic_object(TYPE_INT);
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
	Object *obj = basic_object(TYPE_FLOAT);
	obj->data.d = d;
	return obj;
}

Object* newString(const char *s, int len) {
	Object *obj = basic_object(TYPE_STRING);
	utstring_new(obj->data.str);
	if(len<0) len = strlen(s);
	utstring_bincpy(obj->data.str, s, len);
	return obj;
}

Object* newSymbol(const char *s, int len) {
	Object *obj = basic_object(TYPE_SYMBOL);
	utstring_new(obj->data.str);
	if(len<0) len = strlen(s);
	utstring_bincpy(obj->data.str, s, len);
	return obj;
}

// works for strings OR symbols
int string_length(Object *s) {
	assert(isString(s) || isSymbol(s));
	return utstring_len(s->data.str);
}

// works for strings OR symbols
const char *string_cstr(Object *s) {
	assert(isString(s) || isSymbol(s));
	return utstring_body(s->data.str);
}

Object* newLambda(Object *list) {
	Object *obj = basic_object(TYPE_LAMBDA);
	obj->data.lambda = (Lambda*)x_malloc(sizeof(Lambda));
	obj->data.lambda->list = list;
	obj->data.lambda->outer = NULL;
	return obj;
}

Object *newVoidFunctionPtr(VoidFunctionPtr funcptr) {
	Object *obj = basic_object(TYPE_VOIDFUNCPTR);
	obj->data.funcptr = funcptr;
	return obj;
}

Object* newBoundLambda(Object *list, CallFrameData *data) {
	Object *obj = basic_object(TYPE_LAMBDA);
	obj->data.lambda = (Lambda*)x_malloc(sizeof(Lambda));
	obj->data.lambda->list = list;
	obj->data.lambda->outer = data;
	return obj;
}

Object* newOpcode(uint64_t packed_opcode) {
	Object *obj = basic_object(TYPE_OPCODE);
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

UT_icd Object_icd = { sizeof(Object*), NULL, NULL, NULL };

ObjArray *newObjArray() {
	ObjArray *arr;
	utarray_new(arr, &Object_icd);
	return arr;
}

void ObjArray_append(ObjArray* arr, Object *obj) {
	utarray_push_back(arr, obj);
}

int ObjArray_length(ObjArray* arr) {
	return utarray_len(arr);
}

Object* ObjArray_get(ObjArray* arr, int i) {
	Object *o = utarray_eltptr(arr, i);
	return o;
}
void ObjArray_put(ObjArray* arr, int i, Object *obj) {
	Object *o = utarray_eltptr(arr, i);
	*o = *obj;
}

Object* newList() {
	Object *obj = basic_object(TYPE_LIST);
	obj->data.array = newObjArray();
	return obj;
}

Object* newListKeepArray(ObjArray *array) {
	Object *obj = basic_object(TYPE_LIST);
	obj->data.array = array; // takes ownership of array
	return obj;
}

void List_append(Object *list, Object *obj) {
	ObjArray_append(list->data.array, obj);
}

int List_length(Object *list) {
	return ObjArray_length(list->data.array);
}

Object* List_get(Object *list, int i) {
	return ObjArray_get(list->data.array, i);
}

void List_put(Object *list, int i, Object *obj) {
	ObjArray_put(list->data.array, i, obj);
}

Object *newDict() {
	Object *obj = basic_object(TYPE_DICT);
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

static const char* fmtStackPrintObjArray(ObjArray *arr, char open_delim, char close_delim) {
	UT_string *s;
	utstring_new(s);
	utstring_printf(s, "%c ", open_delim);
	
	Object *obj;
	for(obj=utarray_front(arr); obj != NULL; obj=utarray_next(arr,obj)) {
		utstring_printf(s, "%s ", fmtStackPrint(obj));
	}

	utstring_printf(s, "%c", close_delim);
	return utstring_body(s);
}

int sort_objdictentry_by_name(ObjDictEntry *a, ObjDictEntry *b) {
	return strcmp(a->name, b->name);
}

static CallFrameData *findFrameUp(CallFrameData *frame, int levels) {
	while(levels > 0) {
		if(!frame || !frame->outer)
			error("Bad level number in findFrameUp()");

		levels -= 1;
		frame = frame->outer;
	}
	return frame; // cannot be NULL due to above checks
}

CallFrameData* new_CallFrameData() {
	CallFrameData* cf = (CallFrameData*)x_malloc(sizeof(CallFrameData));
	memset(cf->data, 0, MAX_CALLFRAME_SLOTS*sizeof(Object*));
	cf->outer = NULL;
	cf->bound = 0;
	return cf;
}

Object* callframe_GetFrameObj(CallFrameData *frame, int levels, int index) {
	if(index < 0 || index >= MAX_CALLFRAME_SLOTS)
		error("Out of bounds in callframe_GetFrameObj()");
	// go up number of levels
	frame = findFrameUp(frame, levels);
	return frame->data[index];
}

void callframe_SetFrameObj(CallFrameData *frame, int levels, int index, Object *obj) {
	if(index < 0 || index >= MAX_CALLFRAME_SLOTS)
		error("Out of bounds in callframe_SetFrameObj()");
	// go up number of levels
	frame = findFrameUp(frame, levels);
	frame->data[index] = obj;
}

void callframe_clear(CallFrameData *frame) {
	memset(frame->data, 0, MAX_CALLFRAME_SLOTS*sizeof(CallFrameData*));
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

static ObjArray *deepcopyObjArray(ObjArray *array) {
	ObjArray *newarray = newObjArray();
	for(int i=0; i<ObjArray_length(array); ++i) 
		ObjArray_append(newarray, deepcopy(ObjArray_get(array,i)));
	
	return newarray;
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

		case TYPE_LIST:
			return newListKeepArray(deepcopyObjArray(obj->data.array));

		case TYPE_DICT: {
			Object *newdict = newDict();
			for(ObjDictEntry *ent=obj->data.objdict; ent != NULL; ent = ent->hh.next)
				Dict_put(newdict, ent->name, deepcopy(ent->obj));
			
			return newdict;
		}
	
		default: error("deepcopy not implemented for object type %d", obj->type);
	}
}

const char* fmtStackPrint(Object *obj) {
	UT_string *s;
	utstring_new(s);
	switch(obj->type) {
		case TYPE_NULL: 
			return "<null>";
		case TYPE_VOID: 
			return "<*void*>";
		case TYPE_INT: 
			utstring_printf(s, "%ld", obj->data.i);
			return utstring_body(s);
		case TYPE_FLOAT: 
			utstring_printf(s, "%lf", obj->data.d);
			return utstring_body(s);
		case TYPE_BOOL: 
			return obj->data.i == TRUE ? "<TRUE>" : "<FALSE>";
		case TYPE_LIST:
			return fmtStackPrintObjArray(obj->data.array, '[', ']');
		case TYPE_LAMBDA:
			return fmtStackPrintObjArray(obj->data.lambda->list->data.array, '{', '}');
		case TYPE_STRING:
			utstring_printf(s, "\"%s\"", utstring_body(obj->data.str));
			return utstring_body(s);
		case TYPE_SYMBOL:
			utstring_printf(s, "'%s", utstring_body(obj->data.str));
			return utstring_body(s);
		case TYPE_DICT: 
		{
			HASH_SORT(obj->data.objdict, sort_objdictentry_by_name);
			ObjDictEntry *ent;
			utstring_printf(s, "{ ");
			for(ent=obj->data.objdict; ent != NULL; ent = ent->hh.next) {
				utstring_printf(s, "%s -> %s ", ent->name, fmtStackPrint(ent->obj));
			}
			utstring_printf(s,"}");
			return utstring_body(s);
		}

		default: 
			error("** UNKNOWN TYPE: %d\n", obj->type);
	}
}