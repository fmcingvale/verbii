
/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

// general assumption (here and elsewhere) regarding VINT:
//	* when numeric results are expected, VINTs are preserved
//	* when a VINT is used as a memory index, it is assumed an int (32-bit) is sufficient
//	  so VINTs are often cast to int to avoid compiler warnings.

#include "native.h"
#include "langtypes.h"
#include "errors.h"
#include "xmalloc.h"
#include "util.h"
#include "interpreter.h"
#include "gc_object.h"
#include <math.h>
#include <stdio.h>

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

// NOTE: caller must free returned buffer
char* file_read(const char* filename, int *nrbytes) {
	if(!file_exists(filename))
		error("Trying to read nonexistent file: %s", filename);

	char *buf;
	*nrbytes = file_size(filename);
	buf = (char*)x_malloc((*nrbytes)*sizeof(char));
	
	FILE *fp = fopen(filename, "rb");
	int r = (int)fread(buf, sizeof(char), *nrbytes, fp);
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
	return string_cstr(popStringObj(where));
}

// like above, assumes symbols cannot contain nulls
static const char *popSymbol(const char *where) {
	Object *obj = pop();
	if(!isSymbol(obj))
		error("%s requires symbol, got: %s", where, fmtStackPrint(obj));
	
	return string_cstr(obj);
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

static void pushInt(VINT i) { push(newInt(i)); }
static void pushBool(int b) { push(newBool(b)); }

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
	
	VINT quot = (VINT)floor(((double)(llabs(a))) / ((double)(llabs(b))));
	
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
		heap_set((int)addr->data.i, obj);		
	else
		error("NOT IMPLEMENTED IN set!");
}

// ( addr -- obj ) load obj from addr and push to stack
static void builtin_ref() {
	Object *addr = pop();
	if(isInt(addr))		
		push(heap_get((int)addr->data.i));
	else
		error("ref expects int, got: %s", fmtStackPrint(addr));
}

static void builtin_getsp() { pushInt(get_SP()); }
static void builtin_setsp() { set_SP((int)popInt("SP!")); }

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
	x_free(text);
}

// ( sn .. s1 N -- list of N items; N can be zero for an empty list )
static void builtin_make_list() {
	Object *list = newList();
	int nr = (int)popInt("make-list");
	// instead of adding an insert-type operation which would have to
	// move items on every iteration, pre-fill array then I can put() in 
	// the correct (opposite of stack) order
	for(int i=0; i<nr; ++i)
		List_append(list, newNull());

	for(int i=0; i<nr; ++i) {
		Object *obj = pop();
		List_put(list, nr-i-1, obj);
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

Object* slice_object(Object *obj, int index, int nr) {
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
	int nr = (int)popInt("slice");
	int index = (int)popInt("slice");
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
	//printf("GET FROM %s INDEX %s\n", fmtStackPrint(obj), fmtStackPrint(indexOrKey));	
	if(isString(obj) || isSymbol(obj) || isList(obj)) {
		if(!isInt(indexOrKey))
			error("get expects integer index, got: %s", fmtStackPrint(indexOrKey));
		int index = (int)indexOrKey->data.i;
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
		int index = (int)indexOrKey->data.i;
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

static void builtin_equal() {
	Object *b = pop();
	Object *a = pop();
	pushBool(testEqual(a,b));
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

	return (int)o->data.i;
}

// 'unmake' works such that an immediate 'make-type' would give the 
// unmade object back
static void builtin_unmake() {
	Object *obj = pop();
	// strings & symbols are unmade into ASCII values
	if(isString(obj) || isSymbol(obj)) {
		for(int i=0; i<string_length(obj); ++i)
			pushInt((unsigned char)(string_cstr(obj)[i]));
	
		pushInt(string_length(obj));
	}
	else if(isList(obj)) {
		for(int i=0; i<List_length(obj); ++i)
			push(List_get(obj, i));
		
		pushInt((int)(List_length(obj)));
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
	char *s = (char*)x_malloc(nr*sizeof(char));
	for(int i=0; i<nr; ++i)
		s[nr-i-1] = (char)popInt("make-symbol");

	push(newString(s, nr));
	x_free(s);
}

/* ( cn .. c1 N -- symbol of N chars ) */
static void builtin_make_symbol() {
	int nr = (int)popInt("make-symbol");
	char *s = (char*)x_malloc(nr*sizeof(char));
	for(int i=0; i<nr; ++i)
		s[nr-i-1] = (char)popInt("make-symbol");

	push(newSymbol(s, nr));
	x_free(s);
}

static void builtin_dumpword() {
	// symbols cannot contain NULLs
	const char* symbol = popSymbol(".dumpword");
	Object *wordlist = lookupUserWord(symbol);
	if(isVoid(wordlist))
		error("No such word in .dumpword: %s", symbol);
	
	//printf("DUMPING WORD: %s\n", fmtStackPrint(wordlist));
	push(deepcopy(wordlist));
}

static void builtin_deepcopy() { push(deepcopy(pop())); }

static void builtin_alloc() {
	int count = (int)popInt("alloc");
	pushInt(heap_alloc(count));
}

static void builtin_del() {
	// symbols cannot contain NULLs
	const char* name = popSymbol("del");
	deleteUserWord(name);
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
	pushInt((a&b) & MASK32);
}

static void builtin_bit_or() {
	VINT b = popInt("bit-or");
	VINT a = popInt("bit-or");
	pushInt((a|b) & MASK32);
}

static void builtin_bit_xor() {
	VINT b = popInt("bit-xor");
	VINT a = popInt("bit-xor");
	pushInt((a^b) & MASK32);
}

static void builtin_bit_not() {
	VINT a = popInt("bit-not");
	pushInt((~a) & MASK32);
}

static void builtin_bit_shr() {
	VINT nr = popInt("bit-shr");
	VINT a = popInt("bit-shr");
	// apparently it is undefined behavior to shift >= 32 bits so check for that
	// (without this check, it fails under mingw w/gcc 12)
	if(nr >= 32)
		pushInt(0);
	else
		pushInt((((unsigned long)a)>>nr) & MASK32);
}

static void builtin_bit_shl() {
	VINT nr = popInt("bit-shl");
	VINT a = popInt("bit-shl");
	// workaround as above
	if(nr >= 32)
		pushInt(0);
	else
		pushInt((((unsigned long)a)<<nr) & MASK32);
}

static void builtin_cpu_time() { push(newFloat(current_system_cpu_time() - STARTUP_TIME)); }
static void builtin_new_dict() { push(newDict()); }

static void builtin_file_exists() {
	// filenames cannnot contain NULLs
	const char* filename = popString("file-exists?");
	pushBool(file_exists(filename));
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
	//printf("DESERIALIZE: %s\n", filename);
	FILE *fpin = fopen(filename, "rb");
	deserialize_stream(fpin);
	fclose(fpin);
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
	else {
		char *p = strchr(buf, '\n');
		if(p) *p = 0;
		p = strchr(buf, '\r');
		if(p) *p = 0;
	
		push(newString(buf, (int)strlen(buf)));
	}
}	

#include <time.h>

static void builtin_time_string() {
	char buf[100];
	time_t now;
	time(&now);
	strftime(buf,sizeof(buf)-1,"%Y-%m-%d %H:%M:%S",localtime(&now));
	push(newString(buf,(int)strlen(buf)));
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
		List_append(keys, newString(ent->name, (int)strlen(ent->name)));
	
	push(keys);
}

#include "opcodes.h"

// opcode-name A B C make-opcode
static void builtin_make_opcode() {
	int C = (int)popInt("make-opcode");
	int B = (int)popInt("make-opcode");
	int A = (int)popInt("make-opcode");
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
	pushInt(op->data.opcode);
}

// ( lambda -- bound-lambda )
static void builtin_bind_lambda() {
	Object *lambda = popLambda("bind-lambda");
	//printf("POPPED LAMBDA LIST @ %llx\n", (long long unsigned int)lambda->data.lambda->list);
	// remember currently active frame -- when bound-lambda is called
	// later, this frame will be set as its outer frame
	// (this also marks framedata as bound)
	push(newBoundLambda(lambda->data.lambda->list, framedata));
}

static void builtin_file_pathsep() {
	// need to check what this should be under mingw64 ... for now return as linux default
	push(newString("/",1));
}

static void builtin_f_setprec() {
	FLOAT_PRECISION = (int)popInt("f.setprec");
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
	Object *s = newString(buf,(int)strlen(buf));
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

	for(int i=0; i<string_length(text); ++i) {
		hash ^= (uint32_t)((unsigned char)(string_cstr(text)[i]));
		hash *= 16777619; // FNV prime
	}

	pushInt(hash);
}

static void builtin_type_test(int (*test)(Object*)) {
	Object *obj = pop();
	pushBool((*test)(obj));
}

#define MAKE_TYPE_TEST(name,test) \
	static void builtin_is_##name() { \
		builtin_type_test(test); \
	}

MAKE_TYPE_TEST(int,isInt);
MAKE_TYPE_TEST(float,isFloat);
MAKE_TYPE_TEST(bool,isBool);
MAKE_TYPE_TEST(null,isNull);
MAKE_TYPE_TEST(void,isVoid);
MAKE_TYPE_TEST(list,isList);
MAKE_TYPE_TEST(string,isString);
MAKE_TYPE_TEST(symbol,isSymbol);
MAKE_TYPE_TEST(lambda,isLambda);
MAKE_TYPE_TEST(bound_lambda,isBoundLambda);
MAKE_TYPE_TEST(opcode,isOpcode);

static Object* do_add(Object *a, Object *b) {

	// any cases that aren't handled fall through to error() at end
	switch(a->type) {
		case TYPE_NULL: break;
		case TYPE_INT:
			if(b->type == TYPE_INT) {
				return newInt(a->data.i + b->data.i);
			}
			else if(b->type == TYPE_FLOAT) {
				return newFloat(a->data.i + b->data.d);
			}
			break;
		case TYPE_FLOAT:
			if(b->type == TYPE_INT) {
				return newFloat(a->data.d + b->data.i);
			}
			else if(b->type == TYPE_FLOAT) {
				return newFloat(a->data.d + b->data.d);
			}
			break;
		case TYPE_BOOL: break;
		case TYPE_LAMBDA: break;
		case TYPE_BOUND_LAMBDA: break;
		// strings & symbols defined as immutable, so make new objects
		case TYPE_STRING:
			if(b->type == TYPE_STRING) {
				Object *out = newString(string_cstr(a), string_length(a));
				string_append(out, b);
				return out;
			}	
			break;
		case TYPE_SYMBOL:
			if(b->type == TYPE_SYMBOL) {
				Object *out = newSymbol(string_cstr(a), string_length(a));
				string_append(out, b);
				return out;				
			}	
			break;
		case TYPE_LIST:
			// list + list makes new object (use .extend to add to existing list instead)
			if(b->type == TYPE_LIST) {
				Object *newlist = newList();
				for(int i=0; i<List_length(a); ++i)
					List_append(newlist, List_get(a, i));

				for(int i=0; i<List_length(b); ++i)
					List_append(newlist, List_get(b, i));
					
				return newlist;
			}
			break;

		default: 
			break; // just to be explicit that I mean to fall through
	}
			
	error("Bad operands for +: %s and %s", fmtStackPrint(a), fmtStackPrint(b));
}

static void builtin_add() {
	Object *b = pop();
	Object *a = pop();
	push(do_add(a,b));
}

static Object* do_subtract(Object *a, Object *b) {
	if(a->type == TYPE_INT && b->type == TYPE_INT) {
		return newInt(a->data.i - b->data.i);
	}
	else if(isNumber(a) && isNumber(b)) {
		return newFloat(asNumber(a) - asNumber(b));
	}
	error("Bad operands for -: %s and %s", fmtStackPrint(a), fmtStackPrint(b));
}

static void builtin_subtract() {
	Object *b = pop();
	Object *a = pop();
	push(do_subtract(a,b));
}

static Object *do_multiply(Object *a, Object *b) {
	if(a->type == TYPE_INT && b->type == TYPE_INT) {
		return newInt(a->data.i * b->data.i);
	}
	else if(isNumber(a) && isNumber(b)) {
		return newFloat(asNumber(a) * asNumber(b));
	}
	error("Bad operands for *: %s and %s", fmtStackPrint(a), fmtStackPrint(b));
}

static void builtin_multiply() {
	Object *b = pop();
	Object *a = pop();
	push(do_multiply(a,b));
}

static Object* do_divide(Object *a, Object *b) {
	if(isNumber(a) && isNumber(b)) {
		double denom = asNumber(b);
		if(denom == 0)
			error("Divide by zero");
		
		return newFloat(asNumber(a) / denom);
	}
	error("Bad operands for /: ", fmtStackPrint(a), fmtStackPrint(b));
}

#define MAKE_UNARY_FLOAT_OP(func,name) \
	static void builtin_##func() { \
		push(newFloat(func(popFloatOrInt(name)))); \
	}

MAKE_UNARY_FLOAT_OP(sqrt,"sqrt")
MAKE_UNARY_FLOAT_OP(cos,"cos")
MAKE_UNARY_FLOAT_OP(sin,"sin")
MAKE_UNARY_FLOAT_OP(tan,"tan")
MAKE_UNARY_FLOAT_OP(acos,"acos")
MAKE_UNARY_FLOAT_OP(asin,"asin")
MAKE_UNARY_FLOAT_OP(log,"log")
MAKE_UNARY_FLOAT_OP(exp,"exp")

static void builtin_divide() {
	Object *b = pop();
	Object *a = pop();
	push(do_divide(a,b));
}

static void builtin_length() { pushInt(length(pop())); }
static void builtin_parse_int() { push(parseInt(popStringOrSymbol())); }
static void builtin_parse_float() { push(parseFloat(popStringOrSymbol())); }
static void builtin_void() { push(newVoid()); }
static void builtin_repr() { push(newString(fmtStackPrint(pop()),-1)); }
static void builtin_str() { push(newString(fmtDisplayPrint(pop()),-1)); }

static void builtin_sys_platform() {
	Object *s = newString("",0);
#if defined( __GNUC__)
	string_printf(s, "gcc %d.%d.%d", __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__);
#elif defined(_MSC_VER)
	string_printf(s, "msvc %s", _MSC_FULL_VER);
#endif
	push(newString(string_cstr(s), string_length(s)));
}

static void builtin_set_exit_on_exception() {
	EXIT_ON_EXCEPTION = popBool(); 
}

static void builtin_set_allow_overwrite() {
	ALLOW_OVERWRITING_WORDS = popBool();
}

static void builtin_stacktrace_on_exception() {
	STACKTRACE_ON_EXCEPTION = popBool();
}

static void builtin_floor() { pushInt((VINT)floor(popFloatOrInt("floor"))); }
static void builtin_depth() { pushInt(stack_depth()); }

static void builtin_wordlist() {
	push(getWordlist());
}

static void builtin_puts() {
	if(fp_stdout!=NULL)
		fprintf(fp_stdout, "%s", popString("puts"));
	else
		printf("%s", popString("puts"));
}
	
#include <zlib.h>

static voidpf my_zalloc(voidpf opaque, uInt items, uInt size) {
	//printf("zalloc %d %d\n", items, size);
	return x_malloc(items*size);
}

static void my_zfree(voidpf opaque, voidpf address) {
	//printf("zfree\n");
	x_free(address);
}

// zlib-compress(string, level)
static void builtin_zlib_compress() {
	VINT level = popInt("zlib-compress");
	Object *str_in = pop();
	if(!isString(str_in))
		error("String required in zlib-compress, got: %s\n", fmtStackPrint(str_in));

	z_stream zs;
	zs.zalloc = my_zalloc;
	zs.zfree = my_zfree;
	zs.opaque = NULL;
	if(deflateInit(&zs, level) != Z_OK)
		error("Unexpected return from deflateInit\n");

	// calc upper bound on output size
	unsigned long destLen = compressBound(string_length(str_in));
	char *buf_out = (char*)malloc(destLen);
	// setup buffer pointers
	zs.next_in = (z_const Bytef *)string_cstr(str_in);
	zs.avail_in = string_length(str_in);
	zs.next_out = buf_out;
	zs.avail_out = destLen;

	// this should compress in one shot
	if(deflate(&zs, Z_FINISH) != Z_STREAM_END)
		error("Unexpected return from deflate\n");
		
	Object *str_out = newString(buf_out, zs.total_out);
	//printf("Input len: %lu\n", len);
	//printf("Compressed: %lu\n", zs.total_in);
	//unsigned long csize = zs.total_out;
	//printf("Compressed size: %lu\n", zs.total_out);

	if(deflateEnd(&zs) != Z_OK)
		error("Unexpected return from deflateEnd\n");
	
	push(str_out);
}

// zlib-decompress(string)
static void builtin_zlib_decompress() {
	Object *str_in = pop();
	if(!isString(str_in))
		error("String required in zlib-decompress, got: %s\n", fmtStackPrint(str_in));

	z_stream zs;
	zs.zalloc = my_zalloc;
	zs.zfree = my_zfree;
	zs.opaque = NULL;
	// per docs, these additional fields must be initted before calling inflateInit
	zs.next_in = (z_const Bytef *)string_cstr(str_in);
	zs.avail_in = string_length(str_in);
	if(inflateInit(&zs) != Z_OK)
		error("Unexpected return from inflateInit\n");

	Object *str_out = newString("",0);

	// I don't know the original size, so decompress in chunks
	// (making this too small makes decompression VERY slow)
	int TEMPSIZE = 131072;
	char *TEMPBUF = (char*)x_malloc(TEMPSIZE);
	zs.next_out = TEMPBUF;
	zs.avail_out = TEMPSIZE;
	int rv;
	while(1) {
		//printf("Inflate loop\n");
		// decompress next chunk
		rv = inflate(&zs, 0);
		if(rv == Z_STREAM_END) {
			// collect final output
			string_append_cstr(str_out, TEMPBUF, TEMPSIZE-zs.avail_out);
			break;
		}

		if(rv != Z_OK)
			error("Unexpected error from inflate\n");			

		// take chunk output and append to str_out
		string_append_cstr(str_out, TEMPBUF, TEMPSIZE-zs.avail_out);
		
		// reset the output chunk
		//printf("Got %d bytes\n", 16384 - zs2.avail_out);
		zs.next_out = TEMPBUF;
		zs.avail_out = TEMPSIZE;
	}

	if(inflateEnd(&zs) != Z_OK)
		error("Unexpected return from inflateEnd\n");
		
	x_free(TEMPBUF);

	push(str_out);
}

static void builtin_list_builtins();
typedef struct _BUILTIN_FUNC {
	const char *name;
	void (*fn)();
} BUILTIN_FUNC;

BUILTIN_FUNC BLTINS[] = {
	{ "/mod", builtin_divmod },
	{ ".c", builtin_printchar },
	{ "str", builtin_str },
	{ "repr", builtin_repr },
	{ "puts", builtin_puts },
	{ "SP", builtin_getsp },
	{ "SP!", builtin_setsp },
	{ "depth", builtin_depth },
	{ "ref", builtin_ref },
	{ "set!", builtin_set },
	{ ".dumpword", builtin_dumpword },
	{ "error", builtin_error },
	{ "sys-platform", builtin_sys_platform },
	{ "set-exit-on-exception", builtin_set_exit_on_exception },
	{ "set-allow-overwrite-words", builtin_set_allow_overwrite },
	{ "set-stacktrace-on-exception", builtin_stacktrace_on_exception },
	{ ".wordlist", builtin_wordlist },
	{ ".builtins", builtin_list_builtins },

	{ "f.setprec", builtin_f_setprec },

	{ "==", builtin_equal },
	{ ">", builtin_greater },
	
	{ "+", builtin_add },
	{ "-", builtin_subtract },
	{ "*", builtin_multiply },
	{ "/", builtin_divide },
	
	{ "int?", builtin_is_int },
	{ "float?", builtin_is_float },
	{ "bool?", builtin_is_bool },
	{ "null?", builtin_is_null },
	{ "void?", builtin_is_void },
	{ "list?", builtin_is_list },
	{ "string?", builtin_is_string },
	{ "symbol?", builtin_is_symbol },
	{ "lambda?", builtin_is_lambda },
	{ "bound-lambda?", builtin_is_bound_lambda },
	{ "opcode?", builtin_is_opcode },	
	
	{ "make-list", builtin_make_list },
	{ "make-string", builtin_make_string },
	{ "make-symbol", builtin_make_symbol },
	{ "make-lambda", builtin_make_lambda },
	{ "make-word", builtin_make_word },
	{ "unmake", builtin_unmake },
	{ "slice", builtin_slice },
	{ "append", builtin_append },
	{ "extend", builtin_extend },
	
	{ "length", builtin_length },

	{ "parse-int", builtin_parse_int },
	{ "parse-float", builtin_parse_float },
	{ "void", builtin_void },
	
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

	{ "sqrt", builtin_sqrt },
	{ "cos", builtin_cos },
	{ "sin", builtin_sin },
	{ "tan", builtin_tan },
	{ "acos", builtin_acos },
	{ "asin", builtin_asin },
	{ "log", builtin_log },
	{ "exp", builtin_exp },
	{ "atan2", builtin_atan2 },
	{ "pow", builtin_pow },
	{ "floor", builtin_floor },
	
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

	// zlib
	{ "zlib-compress", builtin_zlib_compress},
	{ "zlib-decompress", builtin_zlib_decompress},

	{NULL, NULL}
};

static void builtin_list_builtins() {
	Object *list = newList();
	int i=0;
	while(1) {
		if(BLTINS[i].name == NULL)
			break; // end of list

		List_append(list, newSymbol(BLTINS[i].name, strlen(BLTINS[i].name)));
		++i;
	}
	push(list);
}

static Object *BUILTINS; // dict

// maps builtin name to index into BLTINS
static Object *BUILTINS_NAME_TO_INDEX;

int lookup_builtin_index(const char *name) {
	Object *index = Dict_get(BUILTINS_NAME_TO_INDEX, name);
	if(isVoid(index))
		return -1;
	else if(isInt(index))
		return index->data.i;
	else
		error("Unexpected error in lookup_builtin_index()");
}

void call_builtin_by_index(int i) {
	if(i < 0 || i >= (sizeof(BLTINS)/sizeof(BLTINS[0])))
		error("Bad builtin index %d", i);

	BLTINS[i].fn();
}

int get_number_of_builtins() {
	if(BUILTINS != NULL)
		return Dict_size(BUILTINS);
	else
		return 0;
}

void init_builtins() {
	BUILTINS = newDict();
	BUILTINS_NAME_TO_INDEX = newDict();
	int i=0;
	while(1) {
		if(BLTINS[i].name == NULL)
			break; // end of list

		//fprintf(stderr, "ADDING BUILTIN: %s\n", BLTINS[i].name);
		// wrap function in an Object for convenience so i can store in a regular dict
		Dict_put(BUILTINS, BLTINS[i].name, newVoidFunctionPtr(BLTINS[i].fn));

		// map name -> index into BLTINS
		Dict_put(BUILTINS_NAME_TO_INDEX, BLTINS[i].name, newInt(i));

		++i;
	}
}

// mark my objects that can't be normally found by the gc
void native_mark_reachable_objects() {
	// mark BUILTINS dict
	if(BUILTINS)
		gc_mark_reachable(BUILTINS);

	if(BUILTINS_NAME_TO_INDEX)
		gc_mark_reachable(BUILTINS_NAME_TO_INDEX);
}
