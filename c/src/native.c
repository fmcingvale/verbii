
/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

//#include "native.h"
#include "langtypes.h"
#include "errors.h"
#include "xmalloc.h"
#include "util.h"
#include "interpreter.h"
#include <math.h>

// file to write output
static FILE *fp_stdout = NULL;

// whether make-word is allowed to overwrite existing words
int ALLOW_OVERWRITING_WORDS = 0;

// scripts can set via set-exit-on-exception to tell host whether
// to restart on exceptions (default is to exit)
int EXIT_ON_EXCEPTION = 1;

// should a stacktrace be printed on exception?
int STACKTRACE_ON_EXCEPTION = 1;

double STARTUP_TIME;

// **NOTE** text MAY contain NULLs, so takes nr bytes to write
void file_write(const char *filename, const char *text, int nr) {
	FILE *fp = fopen(filename,"wb");
	if(!fp)
		error("Unable to create file: %s", filename);

	fwrite(text,sizeof(char),nr,fp);
	fclose(fp);
}

// as above, text is allowed to contain NULLs
void file_append(const char* filename, const char *text, int nr) {
	FILE *fp = fopen(filename,"ab");
	if(!fp)
		error("Unable to append to file: %s", filename);

	fwrite(text,sizeof(char),nr,fp);
	fclose(fp);
}

char* file_read(const char* filename, int *nrbytes) {
	if(!file_exists(filename))
		error("Trying to read nonexistent file: %s", filename);

	char *buf;
	*nrbytes = file_size(filename);
	buf = (char*)x_malloc((*nrbytes)*sizeof(char));
	
	FILE *fp = fopen(filename, "rb");
	int r = fread(buf, sizeof(char), *nrbytes, fp);
	if(r != *nrbytes)
		error("Bad number of bytes read from %s", filename);
	
	fclose(fp);
	return buf;
}

static VINT popInt(const char *where) {
	Object *obj = pop();
	if(!isInt(obj))
		error("%s requires integer, got: %s", where, fmtStackPrint(obj));
	
	return obj->data.i;
}

#if 0 // turned off since currently unused
static double popFloat(, const char *errmsg) {
	Object obj = pop();
	if(!obj.isFloat()) {
		throw LangError(string(errmsg) + " (requires float, got: " + obj.fmtStackPrint() + ")");
	}
	return obj.asFloat();
}
#endif 

static double popFloatOrInt(const char *where) {
	Object *obj = pop();
	if(isFloat(obj))
		return obj->data.d;
	else if(isInt(obj))
		return (double)(obj->data.i);
	else
		error("%s requires number, got: %s", where, fmtStackPrint(obj));
}

static Object* popStringObj(const char *where) {
	Object *obj = pop();
	if(!isString(obj))
		error("%s requires string, got: %s", where, fmtStackPrint(obj));
	
	return obj;
}

// ONLY for cases where the text cannot contain NULLs (i.e. by language definition)
static const char* popString(const char *where) {
	return utstring_body(popStringObj(where)->data.str);
}

// like above, assumes symbols cannot contain nulls
static const char *popSymbol(const char *where) {
	Object *obj = pop();
	if(!isSymbol(obj))
		error("%s requires symbol, got: %s", where, fmtStackPrint(obj));
	
	return utstring_body(obj->data.str);
}

static Object* popList(const char *where) {
	Object *obj = pop();
	if(!isList(obj))
		error("%s requires list, got: %s", where, fmtStackPrint(obj));
	
	return obj;
}

static Object* popLambda(const char *where) {
	Object *obj = pop();
	if(!isLambda(obj))
		error("%s requires lambda, got: %s", where, fmtStackPrint(obj));
	
	return obj;
}

static Object* popDict(const char *where) {
	Object *obj = pop();
	if(!isDict(obj))
		error("%s requires dict, got: %s", where, fmtStackPrint(obj));
	
	return obj;
}

static void pushInt(VINT i) {
	push(newInt(i));
}

static void pushBool(int b) {
	push(newBool(b));
}

/*
	can't count on rounding behavior of host language -- i.e. some languages/systems
	round differently on +/- values.
	 
	for integer divide, I want the WHOLE number of times the divisor goes into the
	dividend (quotient * divisor < dividend) otherwise you end up with negative remainders 
	when you do divmod with this quotient.
	
	also I divide the absolute values and adjust the sign afterwards, to get
	consistent behavior regardless of signs
*/
static void builtin_divmod() {
	VINT b = popInt("divmod");
	VINT a = popInt("divmod");
	VINT mod;

	if(b == 0)
		error("Divide by zero");
	
	VINT quot = (VINT)floor(((double)(abs(a))) / ((double)(abs(b))));

	int samesign = (a < 0 && b < 0) || (a >=0 && b >= 0);
	if(samesign)
		mod = a - quot*b;
	else {
		mod = a + quot*b;
		quot = -quot;
	}

	pushInt(mod);
	pushInt(quot);
}

/* ( list name -- adds word ) */
static void builtin_make_word() {
	//cout << "MAKE-WORD ENTRY:" << intr->reprStack() << endl;
	// names cannot contain NULLs
	const char *name = popSymbol("make-word");
	Object *list = popList("make-word");
	defineWord(name, list, ALLOW_OVERWRITING_WORDS);
}

static void builtin_printchar() {
	int c = (int)popInt("printchar");
	if(fp_stdout != NULL) {
		putc(c, fp_stdout);
		if(c == 10 || c == 13) {
			fflush(fp_stdout);
		}
	}
	else {
		printf("%c", c);
		if(c == 10 || c == 13) {
			fflush(stdout);
		}
	}
}

// ( obj addr -- ) - save obj to addr (index into OBJMEM)
static void builtin_set() {
	Object *addr = pop();
	Object *obj = pop();
	if(isInt(addr))
		heap_set(addr->data.i, obj);		
	else
		error("NOT IMPLEMENTED IN set!");
}

// ( addr -- obj ) load obj from addr and push to stack
static void builtin_ref() {
	Object *addr = pop();
	if(isInt(addr))		
		push(heap_get(addr->data.i));
	else
		error("NOT IMPLEMENTED IN ref");
}

// set stack pointer from addr on stack
// (SP values must be integers)
static void builtin_setsp() {
	set_SP(popInt("SP!"));	
}

static void builtin_error() {
	// messages containing NULLs won't work here
	const char *msg = popString("error"); // do as 2 steps to avoid double-exception
	error("%s", msg);
}

// ( filename -- text )
// reads ALL text from filename
static void builtin_file_read() {
	// filenames containing NULLs won't work here
	const char *filename = popString("file-read");
	if(!file_exists(filename))
		error("No such file in file-read: %s", filename);
	
	int nrbytes;
	char* text = file_read(filename, &nrbytes);
	
	push(newString(text, nrbytes));
}

// ( sn .. s1 N -- list of N items; N can be zero for an empty list )
static void builtin_make_list() {
	Object *list = newList();
	int nr = popInt("make-list");
	for(int i=0; i<nr; ++i) {
		Object *obj = pop();
		List_append(list, obj);
	}
	push(list);
}

static int object_length(Object *obj) {
	if(isString(obj) || isSymbol(obj))
		return string_length(obj);
	else if(isList(obj))
		return List_length(obj);
	else
		error("Object does not support length: %s", fmtStackPrint(obj));
}

Object* slice_object(Object *obj, VINT index, VINT nr) {
	int objsize = object_length(obj);
	
	// adjust index & nr for negative & out of bounds conditions
	if(index < 0) { // index < 0 means count from end
 		index = objsize + index;
	}
	if(index < 0 || index >= objsize) { // out of bounds - return empty object
		if(obj->type == TYPE_STRING)
			return newString("",0);
		else if(obj->type == TYPE_SYMBOL)
			return newSymbol("",0);
		else if(obj->type == TYPE_LIST)
			return newList();
		else
			error("Should never happen");
	}
	if(nr < 0) { // nr < 0 means "copy all, starting at index"
		nr = objsize - index;
	}
	if((index+nr) > objsize) { // past end of object, truncate
		nr = objsize - index;
	}

	switch(obj->type) {
		case TYPE_STRING: return newString(string_cstr(obj)+index, nr);
		case TYPE_SYMBOL: return newSymbol(string_cstr(obj)+index, nr);
		case TYPE_LIST:
			{
				Object *r = newList();
				for(int i=index; i<(index+nr); ++i)
					List_append(r, List_get(obj, i));

				return r;
			}
	}	
	error("Unreachable code");	
}

static void builtin_slice() {
	int nr = popInt("slice");
	int index = popInt("slice");
	Object *obj = pop();
	push(slice_object(obj, index, nr));
}

// ( string i -- i'th char as string )
// ( symbol i -- i'th char as symbol )
// ( list i -- list[i] )
//		i<0 counts from end
// ( dict key -- dict[key] )
//		key must be a string
//
// FOR ALL - returns void if index out of bounds or key does not exist
// only raises error if index/key of wrong type
static void builtin_get() {
	Object* indexOrKey = pop();
	Object* obj = pop();
	if(isString(obj) || isSymbol(obj) || isList(obj)) {
		if(!isInt(indexOrKey))
			error("get expects integer index, got: %s", fmtStackPrint(indexOrKey));
		int index = indexOrKey->data.i;
		if(index < 0) 
			index += object_length(obj); // allow negative indexes to count from end
		if(index < 0 || index >= object_length(obj)) {
			push(newVoid()); // out of bounds == void
		}
		else {
			if(isString(obj))
				push(newString(string_cstr(obj)+index, 1));
			else if(isSymbol(obj))
				push(newSymbol(string_cstr(obj)+index, 1));
			else if(isList(obj))
				push(List_get(obj,index));
		}
	}
	else if(isDict(obj)) {
		if(!isString(indexOrKey))
			error("get expects string key, got: %s", fmtStackPrint(indexOrKey));
		
		push(Dict_get(obj, string_cstr(indexOrKey)));		
	}
	else
		error("get not supported for object: %s", fmtStackPrint(obj));
}

// ( list i obj -- list ; puts obj at list[i] )
// ( dict key obj -- dict ; puts obj at dict[key] )
//		key must be a string
static void builtin_put() {
	Object* obj = pop();
	Object* indexOrKey = pop();
	Object* dest = pop();
	if(isList(dest)) {
		if(!isInt(indexOrKey))
			error("put expects integer index, got: %s", fmtStackPrint(indexOrKey));
		int index = indexOrKey->data.i;
		if(index < 0) index += object_length(dest); // negative indexes count from end
		if(index < 0 || index >= object_length(dest))
			error("index out of range in put");
		List_put(dest, index, obj);
		push(dest);
	}
	else if(isDict(dest)) {
		if(!isString(indexOrKey))
			error("put expects string key, got: %s", fmtStackPrint(indexOrKey));
		// keys cannot contain NULLs
		Dict_put(dest, string_cstr(indexOrKey), obj);
		push(dest);
	}
	else
		error("put not supported for object: %s", fmtStackPrint(dest));
}

// append modifies original object
static void builtin_append() {
	Object* add = pop();

	Object* list = popList("append");
	List_append(list, add);
	push(list);
}

// append items from a list to an existing list, pushing original
// object back to stack
static void builtin_extend() {
	Object* src = popList("extend");
	Object* list = popList("extend");
	for(int i=0; i<List_length(src); ++i)
		List_append(list, List_get(src, i));
	
	push(list);
}

static void builtin_make_lambda() {
	Object* list = popList("make-lambda");
	// must deepcopy list so that external changes to original list cannot
	// affect lambda (see DESIGN-NOTES.md)
	push(newLambda(deepcopy(list)));
}

static void builtin_greater() {
	Object *b = pop();
	Object *a = pop();
	pushBool(testGreater(a,b));
}

// **NOTE** only call this in contexts where the strings cannot contain NULLs
static const char *popStringOrSymbol() {
	Object *o = pop();
	if(isString(o) || isSymbol(o))
		return string_cstr(o);
	else
		error("Expecting string or symbol but got: %s", fmtStackPrint(o));
}

static int popBool() {
	Object *o = pop();
	if(!isBool(o))
		error("Expecting bool but got: %s", fmtStackPrint(o));

	return o->data.i;
}

// 'unmake' works such that an immediate 'make-type' would give the 
// unmade object back
static void builtin_unmake() {
	Object *obj = pop();
	// strings & symbols are unmade into ASCII values
	if(isString(obj) || isSymbol(obj)) {
		for(int i=0; i<string_length(obj); ++i)
			push(newInt((unsigned char)(string_cstr(obj)[i])));
	
		push(newInt(string_length(obj)));
	}
	else if(isList(obj)) {
		for(int i=0; i<List_length(obj); ++i)
			push(List_get(obj, i));
		
		push(newInt((int)(List_length(obj))));
	}
	else if(isLambda(obj)) {
		// push deepcopy of list so it can't be used to modify the lambda 
		// -- see DESIGN-NOTES.md
		push(deepcopy(obj->data.lambda->list));
	}
	else
		error("Object cannot be unmade: %s", fmtStackPrint(obj));
}

/* ( cn .. c1 N -- string of N chars ) */
static void builtin_make_string() {
	int nr = (int)popInt("make-symbol");
	char *s = (char*)malloc(nr*sizeof(char));
	for(int i=0; i<nr; ++i)
		s[nr-i-1] = (char)popInt("make-symbol");

	push(newString(s, nr));
	free(s);
}

/* ( cn .. c1 N -- symbol of N chars ) */
static void builtin_make_symbol() {
	int nr = (int)popInt("make-symbol");
	char *s = (char*)malloc(nr*sizeof(char));
	for(int i=0; i<nr; ++i)
		s[nr-i-1] = (char)popInt("make-symbol");

	push(newSymbol(s, nr));
	free(s);
}

static void builtin_dumpword() {
	// symbols cannot contain NULLs
	const char* symbol = popSymbol(".dumpword");
	Object *wordlist = lookupUserWord(symbol);
	if(isVoid(wordlist))
		error("No such word in .dumpword: %s", symbol);
	
	push(newList(deepcopy(wordlist)));
}

static void builtin_deepcopy() {
	push(deepcopy(pop()));
}

static void builtin_alloc() {
	int count = (int)popInt("alloc");
	push(newInt(heap_alloc(count)));
}

static void builtin_del() {
	// symbols cannot contain NULLs
	const char* name = popSymbol("del");
	deleteWord(name);
}

// ( y x -- atan2[y,x] )
static void builtin_atan2() {
	double x = popFloatOrInt("atan2");
	double y = popFloatOrInt("atan2");
	push(newFloat(atan2(y,x)));
}

// ( x y -- x^y )
static void builtin_pow() {
	double y = popFloatOrInt("atan");
	double x = popFloatOrInt("atan");
	push(newFloat(pow(x,y)));
}

const VINT MASK32 = 0x00000000ffffffff;

static void builtin_bit_and() {
	VINT b = popInt("bit-and");
	VINT a = popInt("bit-and");
	push(newInt((a&b) & MASK32));
}

static void builtin_bit_or() {
	VINT b = popInt("bit-or");
	VINT a = popInt("bit-or");
	push(newInt((a|b) & MASK32));
}

static void builtin_bit_xor() {
	VINT b = popInt("bit-xor");
	VINT a = popInt("bit-xor");
	push(newInt((a^b) & MASK32));
}

static void builtin_bit_not() {
	VINT a = popInt("bit-not");
	push(newInt((~a) & MASK32));
}

static void builtin_bit_shr() {
	VINT nr = popInt("bit-shr");
	VINT a = popInt("bit-shr");
	// apparently it is undefined behavior to shift >= 32 bits so check for that
	// (without this check, it fails under mingw w/gcc 12)
	if(nr >= 32)
		push(newInt(0));
	else
		push(newInt((((unsigned long)a)>>nr) & MASK32));
}

static void builtin_bit_shl() {
	VINT nr = popInt("bit-shl");
	VINT a = popInt("bit-shl");
	// workaround as above
	if(nr >= 32)
		push(newInt(0));
	else
		push(newInt((((unsigned long)a)<<nr) & MASK32));
}

static void builtin_cpu_time() {
	push(newFloat(current_system_cpu_time() - STARTUP_TIME));
}

static void builtin_new_dict() {
	push(newDict());
}

static void builtin_file_exists() {
	// filenames cannnot contain NULLs
	const char* filename = popString("file-exists?");
	push(newBool(file_exists(filename)));
}

// ( filename -- ; open filename and write stdout there )
// ( void -- ; close any file attached to stdout and reset to normal stdout )
//
// this only redirects builtins 'puts' and '.c'. this does NOT redirect error messages
// and (builtin) prompts, they still go to the screen.
static void builtin_open_as_stdout() {
	Object *obj = pop();
	if(isVoid(obj)) {
		if(fp_stdout!=NULL) {
			fflush(fp_stdout);
			fclose(fp_stdout);
			fp_stdout = NULL;
		}
	}
	else if(isString(obj)) {
		if(fp_stdout != NULL) {
			fflush(fp_stdout);
			fclose(fp_stdout);
		}
		// filenames with NULLs not supported
		fp_stdout = fopen(string_cstr(obj), "w");
	}
	else
		error("Unknown arg to open-as-stdout: %s", fmtStackPrint(obj));
}

#include "deserialize.h"

static void builtin_deserialize() {
	// filename cannot contain NULLs
	const char *filename = popString("deserialize");
	FILE *fpin = fopen(filename, "rb");
	deserialize_stream(fpin);
	// no return, just loads words into interpreter
}

#include <stdio.h>

static void builtin_prompt() {
	const char *prompt = popString("prompt");
	// NOTE - ignore any user-set stdout since user needs to see prompt on screen
	printf("%s", prompt);
	fflush(stdout);
	char buf[256];
	
	if(!fgets(buf, 255, stdin))
		push(newVoid());
	else
		push(newString(buf, strlen(buf)));
}	

#include <time.h>

static void builtin_time_string() {
	char buf[100];
	time_t now;
	time(&now);
	strftime(buf,sizeof(buf)-1,"%Y-%m-%d %H:%M:%S",localtime(&now));
	push(newString(buf,strlen(buf)));
}

// ( filename string -- )
// write string to filename, overwriting existing file
static void builtin_file_write() {
	// text MAY contain NULLs, so I need the string object, not the char*
	Object *text = popStringObj("file-write");
	// filenames may not contain NULLs
	const char *filename = popString("file-write");
	file_write(filename, string_cstr(text), string_length(text));
}

// ( filename string -- )
// append string to end of file (or start new file)
static void builtin_file_append() {
	// like above, text MAY contain NULLs
	Object *text = popStringObj("file-append");
	// but filename may not
	const char *filename = popString("file-append");
	file_append(filename, string_cstr(text), string_length(text));
}

// ( filename -- )
// delete file if it exists (no error if it does not exist)
static void builtin_file_delete() {
	// filename may not contain nulls
	const char *filename = popString("file-append");
	remove(filename);
}

// NOT required to be sorted
static void builtin_keys() {
	Object *obj = popDict("keys");
	Object *keys = newList();
	for(ObjDictEntry *ent = obj->data.objdict; ent !=  NULL; ent = ent->hh.next)
		List_append(keys, newString(ent->name, strlen(ent->name)));
	
	push(keys);
}

#include "opcodes.h"

// opcode-name A B C make-opcode
static void builtin_make_opcode() {
	int C = popInt("make-opcode");
	int B = popInt("make-opcode");
	int A = popInt("make-opcode");
	const char *name = popStringOrSymbol();

	// range checks
	if(A < 0 || A > 255)
		error("A must be [0-255] in make-opcode, got: %d", A);

	if(B < 0 || B > 65535)
		error("B must be [0-65535] in make-opcode, got: %d", B);

	if(C < 0 || C > 0x000fffff)
		error("C must be [0-1048575] in make-opcode, got: %d", C);

	push(newOpcode(opcode_pack(opcode_name_to_code(name), A, B, C)));
}		

static void builtin_opcode_packed() {
	Object *op = pop();
	if(!isOpcode(op))
		error("Expecting opcode in opcode-packed but got: %s", fmtStackPrint(op));

	// this is (for example) why opcodes need to fit into 52-bits -- so they can be moved
	// around as regular ints and not some wrapped object
	push(newInt(op->data.opcode));
}

// ( lambda -- bound-lambda )
static void builtin_bind_lambda() {
	Object *lambda = popLambda("bind-lambda");
	// remember currently active frame -- when bound-lambda is called
	// later, this frame will be set as its outer frame
	push(newBoundLambda(lambda, framedata));
	// mark current frame as being bound now so it isn't freed
	framedata->bound = TRUE;
}

static void builtin_file_pathsep() {
	// need to check what this should be under mingw64 ... for now return as linux default
	push(newString("/",1));
}

#ifdef _MSC_VER
#include <direct.h>
#else
#include <stdlib.h>
#include <unistd.h>
#include <limits.h>
#endif

#ifdef _MSC_VER
static void builtin_os_getcwd() {
	char* buf = _getcwd(NULL, 0);
	if (!buf)
		error("Error in getcwd()");

	// assumes path cannot contain NULLs
	Object *s = newString(buf,strlen(buf));
	free(buf);
	push(s);
}
#else // assume posix
static void builtin_os_getcwd() {
	char buf[PATH_MAX+1];
	if(!getcwd(buf, PATH_MAX))
		error("Error in getcwd()");
	
	// assumes path cannot contain NULLs
	push(newString(buf,strlen(buf)));
}
#endif

// FNV-1a 32-bit hash
// ref: https://en.wikipedia.org/wiki/Fowler%E2%80%93Noll%E2%80%93Vo_hash_function
//
// NOTE: In real-world usage, FNV-1a appears to have been superceded by SipHash in many
// popular languages due to hash-flooding DOS attacks. However, the primary usage of 
// FNV-1a in verbii is as a file checksum, so not intended to be used against malicious 
// attacks. The simplicity of implementing FNV-1a across ports is the primary consideration here.
static void builtin_fnv_1a_32() {
	// string is allowed to contain NULLs, so I need the string object NOT the char*
	Object *text = popStringObj("fnv-1a-32");
	uint32_t hash = 2166136261; // offset basis
	size_t i;

	for(int i=0; i<string_length(text); ++i) {
		hash ^= (uint32_t)((unsigned char)(string_cstr(text)[i]));
		hash *= 16777619; // FNV prime
	}

	push(newInt(hash));
}

typedef struct _BUILTIN_FUNC {
	const char *name;
	void (*fn)();
} BUILTIN_FUNC;

BUILTIN_FUNC BLTINS[] = {
	{"/mod", builtin_divmod},
	{".c", builtin_printchar},
	{ "SP!", builtin_setsp },
	{ "ref", builtin_ref },
	{ "set!", builtin_set },
	{ ".dumpword", builtin_dumpword },
	{ "error", builtin_error },

	{ ">", builtin_greater },

	{ "make-list", builtin_make_list },
	{ "make-string", builtin_make_string },
	{ "make-symbol", builtin_make_symbol },
	{ "make-lambda", builtin_make_lambda },
	{ "make-word", builtin_make_word },
	{ "unmake", builtin_unmake },
	{ "slice", builtin_slice },
	{ "append", builtin_append },
	{ "extend", builtin_extend },
	
	{ "put", builtin_put },
	{ "get", builtin_get },
	{ "deepcopy", builtin_deepcopy },
	{ "alloc", builtin_alloc },
	{ ",,del", builtin_del },

		// bitops - defined with long names so user can pick their own shorthand
	{ "bit-and", builtin_bit_and },
	{ "bit-or", builtin_bit_or },
	{ "bit-not", builtin_bit_not },
	{ "bit-xor", builtin_bit_xor },
	{ "bit-shr", builtin_bit_shr },
	{ "bit-shl", builtin_bit_shl },

	{ "cpu-time", builtin_cpu_time },
	{ ",,new-dict", builtin_new_dict },
	{ "keys", builtin_keys},

		// new words needed for running boot.verb
	{ "file-exists?", builtin_file_exists },
	{ "open-as-stdout", builtin_open_as_stdout },
	{ "deserialize", builtin_deserialize },
	{ "prompt", builtin_prompt },

	{ "file-write", builtin_file_write },
	{ "file-append", builtin_file_append },
	{ "file-read", builtin_file_read },
	{ "file-delete", builtin_file_delete },

	{ "atan2", builtin_atan2},
	{ "pow", builtin_pow},
	
	{ "time-string", builtin_time_string },
		
	// fast hashing
	{ "fnv-1a-32", builtin_fnv_1a_32},

	// 'version 2' closures
	{ "make-opcode", builtin_make_opcode},
	{ "opcode-packed", builtin_opcode_packed},
	{ "bind-lambda", builtin_bind_lambda},

	// more os/fileops
	{ "file-pathsep", builtin_file_pathsep},
	{ "os-getcwd", builtin_os_getcwd},

	{NULL, NULL}
};

Object *BUILTINS; // dict

void init_builtins() {
	BUILTINS = newDict();
	int i=0;
	while(1) {
		if(BLTINS[i].name == NULL)
			break; // end of list

		// wrap function in an Object for convenience so i can store in a regular dict
		Dict_put(BUILTINS, BLTINS[i].name, newVoidFunctionPtr(BLTINS[i].fn));
		++i;
	}
}

#if 0
std::map<std::string,BUILTIN_FUNC> BUILTINS { 
	{"+", []() { do_binop(intr, &Object::opAdd); }},
	{"-", []() { do_binop(intr, &Object::opSubtract); }},
	{"*", []() { do_binop(intr, &Object::opMul); }},
	{"/", []() { do_binop(intr, &Object::opDivide); }},
	{"f.setprec", []() { FLOAT_PRECISION = popInt( "Bad arg to f.setprec");}},
	{"puts", []() 
		{
			if(fp_stdout!=NULL)
				fprintf(fp_stdout, "%s", popString(intr,"puts"));
			else
				printf("%s", popString(intr,"puts"));
		}},
	{"puts-stderr", []() 
		{
			fprintf(stderr, "%s", popString(intr,"puts"));			
		}},
		// - NOTE - repr & str COULD be implemented in verbii, however, they have to be
		//          implemented natively anyways for internal error printing, so
		//          no purpose in implementing twice

		// convert TOS to verbose printable string (like for stack display)
		{ "repr", [](Interpreter* intr) {push(newString(pop().fmtStackPrint())); } },
			// convert TOS to normal printable string (like for '.')
		{ "str", [](Interpreter* intr) {push(newString(pop().fmtDisplay())); } },
		{ "==", [](Interpreter* intr) {pushBool(intr, pop().opEqual(pop())); } },
		
		{ "int?", [](Interpreter* intr) {push(newBool(pop().isInt())); } },
		{ "float?", [](Interpreter* intr) {push(newBool(pop().isFloat())); } },
		{ "bool?", [](Interpreter* intr) {push(newBool(pop().isBool())); } },
		{ "null?", [](Interpreter* intr) {push(newBool(pop().isNull())); } },
		{ "void?", [](Interpreter* intr) {push(newBool(pop().isVoid())); } },
		{ "list?", [](Interpreter* intr) {push(newBool(pop().isList())); } },
		{ "string?", [](Interpreter* intr) {push(newBool(pop().isString())); } },
		{ "symbol?", [](Interpreter* intr) {push(newBool(pop().isSymbol())); } },
		{ "lambda?", [](Interpreter* intr) {push(newBool(pop().isLambda())); } },
		{ "bound-lambda?", [](Interpreter* intr) {push(newBool(pop().isBoundLambda())); } },
		{ "opcode?", [](Interpreter* intr) {push(newBool(pop().isOpcode())); } },

		{ "length", [](Interpreter* intr) {push(pop().opLength()); } },
		{ "SP", [](Interpreter* intr) {push(newInt(intr->SP)); } },
		{ ".wordlist", [](Interpreter* intr) {push(intr->getWordlist()); } },
		
			// could implement next two in script, however, host language has to have this
			// function anyways to deserialize programs, so just use that
		{ "parse-int", [](Interpreter* intr) {push(parseInt(popStringOrSymbol(intr))); } },
		{ "parse-float", [](Interpreter* intr) {push(parseFloat(popStringOrSymbol(intr))); } },
		{ "void", [](Interpreter* intr) {push(newVoid()); } },

		{ "set-exit-on-exception", [](Interpreter* intr) {EXIT_ON_EXCEPTION = popBool(intr); } },
		{ "set-allow-overwrite-words", [](Interpreter* intr) {ALLOW_OVERWRITING_WORDS = popBool(intr); } },
		{ "set-stacktrace-on-exception", [](Interpreter* intr) {STACKTRACE_ON_EXCEPTION = popBool(intr); } },

			// more words added while making the random module
			// this is commonly defined as returning a float, but i'm defining it to return an int --
			// will make no difference in any math operation and this allows the result to be used
			// in an integer context
		{ "floor", [](Interpreter* intr) {push(newInt((VINT)floor(popFloatOrInt(intr,"floor")))); } },

	
#if defined( __GNUC__)
		{ "sys-platform", [](Interpreter* intr) {push(newString(
			 string("g++ ") + to_string(__GNUC__) + "." +
				to_string(__GNUC_MINOR__) + "." + to_string(__GNUC_PATCHLEVEL__))); } },
#elif defined(_MSC_VER)
		{ "sys-platform", [](Interpreter* intr) {push(newString(
			 string("msvc++ ") + to_string(_MSC_FULL_VER))); } },
#endif
		{"depth", [](){push(newInt(intr->SP_EMPTY - intr->SP));}},

	
	// more math functions
	{"sqrt", [](){push(newFloat(sqrt(popFloatOrInt(intr, "sqrt"))));}},
	{"cos", [](){push(newFloat(cos(popFloatOrInt(intr, "cos"))));}},
	{"sin", [](){push(newFloat(sin(popFloatOrInt(intr, "sin"))));}},
	{"tan", [](){push(newFloat(tan(popFloatOrInt(intr, "tan"))));}},
	{"acos", [](){push(newFloat(acos(popFloatOrInt(intr, "cos"))));}},
	{"asin", [](){push(newFloat(asin(popFloatOrInt(intr, "sin"))));}},
	// natural log
	{"log", [](){push(newFloat(log(popFloatOrInt(intr, "log"))));}},
	{"exp", [](){push(newFloat(exp(popFloatOrInt(intr, "exp"))));}},
	
	
};
#endif
