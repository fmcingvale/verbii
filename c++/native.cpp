/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "native.hpp"
#include "errors.hpp"
#include "xmalloc.hpp"
#include <cmath>
#include <string>
#include <vector>
#include <map>
#include <iostream>
#include <sys/stat.h>
using namespace std;

// file to write output
static FILE *fp_stdout = stdout;

// whether make-word is allowed to overwrite existing words
bool ALLOW_OVERWRITING_WORDS = false;

// scripts can set via set-exit-on-exception to tell host whether
// to restart on exceptions (default is to exit)
bool EXIT_ON_EXCEPTION = true;

// should a stacktrace be printed on exception?
bool STACKTRACE_ON_EXCEPTION = true;

std::chrono::time_point<std::chrono::steady_clock> STARTUP_TIME;

static VINT popInt(Interpreter *intr, const char *errmsg) {
	Object obj = intr->pop();
	if(!obj.isInt()) {
		throw LangError(string(errmsg) + " (requires integer, got: " + obj.fmtStackPrint() + ")");
	}
	return obj.asInt();
}

#if 0 // turned off since currently unused
static double popFloat(Interpreter *intr, const char *errmsg) {
	Object obj = intr->pop();
	if(!obj.isFloat()) {
		throw LangError(string(errmsg) + " (requires float, got: " + obj.fmtStackPrint() + ")");
	}
	return obj.asFloat();
}
#endif 

static double popFloatOrInt(Interpreter *intr, const char *errmsg) {
	Object obj = intr->pop();
	if(obj.isFloat())
		return obj.asFloat();
	else if(obj.isInt())
		return (float)obj.asInt();
	else
		throw LangError(string(errmsg) + " (requires integer, got: " + obj.fmtStackPrint() + ")");
}

static const char *popString(Interpreter *intr, const char *errmsg) {
	Object obj = intr->pop();
	if(!obj.isString()) {
		throw LangError(string(errmsg) + " (requires string, got: " + obj.fmtStackPrint() + ")");
	}
	return obj.asString();
}

static const char *popSymbol(Interpreter *intr, const char *errmsg) {
	Object obj = intr->pop();
	if(!obj.isSymbol()) {
		throw LangError(string(errmsg) + " (requires symbol, got: " + obj.fmtStackPrint() + ")");
	}
	return obj.asSymbol();
}

static Object popList(Interpreter *intr, const char *errmsg) {
	Object obj = intr->pop();
	if(!obj.isList()) {
		throw LangError(string(errmsg) + " (requires list, got: " + obj.fmtStackPrint() + ")");
	}
	return obj;
}

static Object popDict(Interpreter *intr, const char *where) {
	Object obj = intr->pop();
	if(!obj.isDict()) {
		throw LangError(string(where) + " requires dict, got: " + obj.fmtStackPrint());
	}
	return obj;
}

static void pushInt(Interpreter *intr, VINT i) {
	intr->push(newInt(i));
}

static void pushBool(Interpreter *intr, bool b) {
	intr->push(newBool(b));
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
static void builtin_divmod(Interpreter *intr) {
	VINT b = popInt(intr, "Bad divmod denominator");
	VINT a = popInt(intr, "Bad divmod numerator");
	VINT mod;

	if(b == 0) {
		throw LangError("Divide by zero");
	}
	VINT quot = (VINT)floor(((double)(abs(a))) / ((double)(abs(b))));

	bool samesign = (a < 0 && b < 0) || (a >=0 && b >= 0);
	if(samesign) {
		mod = a - quot*b;
	}
	else {
		mod = a + quot*b;
		quot = -quot;
	}

	pushInt(intr, mod);
	pushInt(intr, quot);
}

/* ( list name -- adds word ) */
static void builtin_make_word(Interpreter *intr) {
	//cout << "MAKE-WORD ENTRY:" << intr->reprStack() << endl;
	const char *name = popSymbol(intr, "make-word bad name");
	Object list = popList(intr, "make-word bad list");
	intr->defineWord(name, list.data.objlist, ALLOW_OVERWRITING_WORDS);
}

static void builtin_printchar(Interpreter *intr) {
	int c = (int)popInt(intr, "Bad printchar arg");
	putc(c, fp_stdout);
	if(c == 10 || c == 13) {
		fflush(fp_stdout);
	}
}

// ( obj addr -- ) - save obj to addr (index into OBJMEM)
static void builtin_set(Interpreter *intr) {
	Object addr = intr->pop();
	Object obj = intr->pop();
	if(addr.isInt()) {
		// SP or LP index
		int index = (int)addr.asInt();
		if(index < 0 || index > intr->HEAP_END) {
			throw LangError("Bad address in set!: " + to_string(index));
		}
		intr->OBJMEM[index] = obj;
	}
	else {
		throw LangError("NOT IMPLEMENTED IN set!");
	}
}

// ( addr -- obj ) load obj from addr and push to stack
static void builtin_ref(Interpreter *intr) {
	Object addr = intr->pop();
	if(addr.isInt()) {
		int index = (int)addr.asInt();
		if(index < 0 || index > intr->HEAP_END) {
			throw LangError("Bad address in ref: " + to_string(index));
		}
		intr->push(intr->OBJMEM[index]);
	}
	else {
		throw LangError("NOT IMPLEMENTED IN ref");
	}
}

// set stack pointer from addr on stack
// (SP values must be integers)
static void builtin_setsp(Interpreter *intr) {
	int addr = (int)popInt(intr, "Bad SP! arg");
	if(addr < intr->SP_MIN || addr > intr->SP_EMPTY) {
		throw LangError("Bad address in SP!: " + to_string(addr));
	}
	intr->SP = addr;
	// stats
	intr->min_run_SP = min(intr->min_run_SP, intr->SP);
}

// set locals pointer from addr on stack
// (LP values must be integers)
static void builtin_setlp(Interpreter *intr) {
	int addr = (int)popInt(intr, "Bad LP! arg");
	if(addr < intr->LP_MIN || addr > intr->LP_EMPTY) {
		throw LangError("Bad address in LP!: " + to_string(addr));
	}
	intr->LP = addr;
	// stats
	intr->min_run_LP = min(intr->min_run_LP, intr->LP);
}

// pop top of stack and push to locals
static void builtin_tolocal(Interpreter *intr) {
	if(intr->LP <= intr->LP_MIN) {
		throw LangError("Locals overflow");
	}
	intr->OBJMEM[--intr->LP] = intr->pop();
}

// pop top locals and push to stack
static void builtin_fromlocal(Interpreter *intr) {
	if(intr->LP >= intr->LP_EMPTY) {
		throw LangError("Locals underflow");
	}
	intr->push(intr->OBJMEM[intr->LP++]);
}

static void builtin_error(Interpreter *intr) {
	const char *msg = popString(intr, "error requires message"); // do as 2 steps to avoid double-exception
	throw LangError(msg);
}

static void do_binop(Interpreter *intr, Object (Object::*op)(const Object &) const) {
	Object b = intr->pop();
	Object a = intr->pop();
	intr->push((a.*op)(b));
}

#include <iostream>
#include <fstream>

// ( filename -- text )
// reads ALL text from filename
static void builtin_file_read(Interpreter *intr) {
	const char *filename = popString(intr, "read-file missing filename");
	ifstream fileIn(filename, ios::binary);
	if(fileIn.rdstate() != ios_base::goodbit) {
		throw LangError("No such file in read-file: " + string(filename));
	}
	fileIn.seekg(0, ios::end);
	size_t len = fileIn.tellg();
	fileIn.seekg(0, ios::beg);
	char *buf = (char*)x_malloc(len);
	fileIn.read(buf, len);
	fileIn.close();

	intr->push(newString(buf, len, true)); // give pointer away
}

// ( sn .. s1 N -- list of N items; N can be zero for an empty list )
static void builtin_make_list(Interpreter *intr) {
	Object list = newList();
	int nr = (int)popInt(intr, "Bad make-list number of items");
	for(int i=0; i<nr; ++i) {
		list.data.objlist->insert(list.data.objlist->begin(), intr->pop());
	}
	intr->push(list);
}

static void builtin_slice(Interpreter *intr) {
	int nr = (int)popInt(intr, "Bad slice count");
	int index = (int)popInt(intr, "Bad slice index" );
	Object obj = intr->pop();
	intr->push(obj.opSlice(index, nr));
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
static void builtin_get(Interpreter *intr) {
	auto indexOrKey = intr->pop();
	auto obj = intr->pop();
	if(obj.isString() || obj.isSymbol() || obj.isList()) {
		if(!indexOrKey.isInt())
			throw LangError("get expects integer index, got: " + indexOrKey.fmtStackPrint());
		int index = indexOrKey.asInt();
		if(index < 0) 
			index += obj.length(); // allow negative indexes to count from end
		if(index < 0 || index >= obj.length()) {
			intr->push(newVoid()); // out of bounds == void
		}
		else {
			if(obj.isString())
				intr->push(newString(obj.asString()+index, 1));
			else if(obj.isSymbol())
				intr->push(newSymbol(obj.asSymbol()+index, 1));
			else if(obj.isList())
				intr->push(obj.asList()->at(index));
		}
	}
	else if(obj.isDict()) {
		if(!indexOrKey.isString())
			throw LangError("get expects string key, got: " + indexOrKey.fmtStackPrint());
		auto entry = obj.asDict()->find(indexOrKey.asString());
		if(entry == obj.asDict()->end())
			// no such key -> void (since void is never a valid stored object)
			intr->push(newVoid());
		else
		 	intr->push((*obj.asDict())[indexOrKey.asString()]);
	}
	else
		throw LangError("get not supported for object: " + obj.fmtStackPrint());
}

// ( list i obj -- list ; puts obj at list[i] )
// ( dict key obj -- dict ; puts obj at dict[key] )
//		key must be a string
static void builtin_put(Interpreter *intr) {
	auto obj = intr->pop();
	auto indexOrKey = intr->pop();
	auto dest = intr->pop();
	if(dest.isList()) {
		if(!indexOrKey.isInt())
			throw LangError("put expects integer index, got: " + indexOrKey.fmtStackPrint());
		int index = indexOrKey.asInt();
		if(index < 0) index += (int)dest.asList()->size(); // negative indexes count from end
		if(index < 0 || index >= (int)dest.asList()->size())
			throw LangError("index out of range in put");
		dest.asList()->at(index) = obj;
		intr->push(dest);
	}
	else if(dest.isDict()) {
		if(!indexOrKey.isString())
			throw LangError("put expects string key, got: " + indexOrKey.fmtStackPrint());
		(*dest.asDict())[indexOrKey.asString()] = obj;
		intr->push(dest);
	}
	else
		throw LangError("put not supported for object: " + dest.fmtStackPrint());
}

// append modifies original object
static void builtin_append(Interpreter *intr) {
	Object add = intr->pop();
	Object list = popList(intr, "Bad arg to append");
	list.data.objlist->push_back(add);
	intr->push(list);
}

static void builtin_make_lambda(Interpreter *intr) {
	Object list = popList(intr, "Bad arg to make-lambda");
	// must deepcopy list so that external changes to original list cannot
	// affect lambda (see DESIGN-NOTES.md)
	intr->push(newLambda(list.deepcopy().data.objlist));
}

static void builtin_greater(Interpreter *intr) {
	Object b = intr->pop();
	Object a = intr->pop();
	pushBool(intr, a.opGreater(b));
}

static const char *popStringOrSymbol(Interpreter *intr) {
	Object o = intr->pop();
	if(o.isString())
		return o.asString();
	else if(o.isSymbol())
		return o.asSymbol();
	else
		throw LangError("Expecting string or symbol but got: " + o.fmtStackPrint());
}

static bool popBool(Interpreter *intr) {
	Object o = intr->pop();
	if(!o.isBool())
		throw LangError("Expecting bool but got: " + o.fmtStackPrint());

	return o.asBool();
}

// 'unmake' works such that an immediate 'make-type' would give the 
// unmade object back
static void builtin_unmake(Interpreter *intr) {
	Object obj = intr->pop();
	// strings & symbols are unmade into ASCII values
	if(obj.isString() || obj.isSymbol()) {
		int len = strlen(obj.data.str);
		for(int i=0; i<len; ++i) {
			intr->push(newInt((int)(obj.data.str[i])));
		}
		intr->push(newInt(len));
	}
	else if(obj.isList()) {
		for(auto obj : *obj.data.objlist) {
			intr->push(obj);
		}
		intr->push(newInt((int)(obj.data.objlist->size())));
	}
	else if(obj.isLambda()) {
		// push deepcopy of list so it can't be used to modify the lambda 
		// -- see DESIGN-NOTES.md
		intr->push(newList(deepcopy(obj.data.objlist)));
	}
	else if(obj.isClosure()) {
		// break into ( list state ) so make-closure would work
		// as above, push a deepcopy of list so it can't be used to modify the closure
		intr->push(newList(deepcopy(obj.asClosureFunc())));
		intr->push(obj.asClosureState());
	}
	else
		throw LangError("Object cannot be unmade: " + obj.fmtStackPrint());
}

/* ( cn .. c1 N -- string of N chars ) */
static void builtin_make_string(Interpreter *intr) {
	int nr = (int)popInt(intr, "Bad count in make-string");
	string s = "";
	for(int i=0; i<nr; ++i) {
		s.insert(s.begin(),(char)popInt(intr, "Bad char in make-string"));
	}
	intr->push(newString(s));
}

/* ( cn .. c1 N -- symbol of N chars ) */
static void builtin_make_symbol(Interpreter *intr) {
	int nr = (int)popInt(intr, "Bad count in make-symbol");
	string s = "";
	for(int i=0; i<nr; ++i) {
		s.insert(s.begin(),(char)popInt(intr, "Bad char in make-symbol"));
	}
	intr->push(newSymbol(s));
}

static void builtin_dumpword(Interpreter *intr) {
	const char* symbol = popSymbol(intr,"Bad name in .dumpword");
	ObjList *wordlist = intr->lookup_word(symbol);
	if(!wordlist) {
		throw LangError("No such word in .dumpword: " + string(symbol));
	}
	intr->push(newList(deepcopy(wordlist)));
}

#include "deserialize.hpp"

static void builtin_loadc(Interpreter *intr) {
	const char *filename = popString(intr, "Bad filename for .loadc");
	ifstream fileIn(filename);
	deserialize_stream(intr, fileIn);
}

static void builtin_make_closure(Interpreter *intr) {
	Object state = intr->pop();
	Object obj = intr->pop();
	// can be called with [ .. ] or { .. } as function
	if(obj.isList())
		// must deepcopy lists so future changes to original list do not affect this
		intr->push(newClosure(deepcopy(obj.asList()), state));
	else if(obj.isLambda())
		// as above
		intr->push(newClosure(deepcopy(obj.asLambda()), state));
	else
		throw LangError("make-closure expects list or lambda, got:" + obj.fmtStackPrint());
}
	
static void builtin_self_get(Interpreter *intr) {
	if(!intr->closure)
		throw LangError("Attempting to reference unbound self");
	
	intr->push(intr->closure->state);
}

static void builtin_self_set(Interpreter *intr) {
	if(!intr->closure)
		throw LangError("Attempting to set unbound self");
	
	intr->closure->state = intr->pop();
}

static void builtin_deepcopy(Interpreter *intr) {
	intr->push(intr->pop().deepcopy());
}

static void builtin_alloc(Interpreter *intr) {
	auto count = (int)popInt(intr, "bad count in alloc");
	intr->push(newInt(intr->heap_alloc(count)));
}

static void builtin_del(Interpreter *intr) {
	auto name = popSymbol(intr, "bad name in del");
	intr->deleteWord(name);
}

const VINT MASK32 = 0x00000000ffffffff;

static void builtin_bit_and(Interpreter *intr) {
	auto b = popInt(intr,"bit-and");
	auto a = popInt(intr,"bit-and");
	intr->push(newInt((a&b) & MASK32));
}

static void builtin_bit_or(Interpreter *intr) {
	auto b = popInt(intr,"bit-or");
	auto a = popInt(intr,"bit-or");
	intr->push(newInt((a|b) & MASK32));
}

static void builtin_bit_xor(Interpreter *intr) {
	auto b = popInt(intr,"bit-xor");
	auto a = popInt(intr,"bit-xor");
	intr->push(newInt((a^b) & MASK32));
}

static void builtin_bit_not(Interpreter *intr) {
	auto a = popInt(intr,"bit-not");
	intr->push(newInt((~a) & MASK32));
}

static void builtin_bit_shr(Interpreter *intr) {
	auto nr = popInt(intr,"bit-shr");
	auto a = popInt(intr,"bit-shr");
	intr->push(newInt((((unsigned long)a)>>nr) & MASK32));
}

static void builtin_bit_shl(Interpreter *intr) {
	auto nr = popInt(intr,"bit-shl");
	auto a = popInt(intr,"bit-shl");
	intr->push(newInt((((unsigned long)a)<<nr) & MASK32));
}

static void builtin_run_time(Interpreter *intr) {
	auto current = chrono::steady_clock::now();
	chrono::duration<double> diff = current - STARTUP_TIME;
	intr->push(newFloat(diff.count()));
}

static void builtin_new_dict(Interpreter *intr) {
	intr->push(newDict());
}

static void builtin_file_exists(Interpreter *intr) {
	auto filename = popString(intr,"file-exists?");
	struct stat st;
	if(stat(filename, &st) < 0)
		intr->push(newBool(false));
	else
		intr->push(newBool(S_ISREG(st.st_mode) != 0));
}

static void builtin_file_mtime(Interpreter *intr) {
	auto filename = popString(intr,"file-exists?");
	struct stat st;
	if(stat(filename, &st) < 0)
		throw LangError("No such file: " + string(filename));
	else
		intr->push(newInt(st.st_mtime));
}

// ( filename -- ; open filename and write stdout there )
// ( void -- ; close any file attached to stdout and reset to normal stdout )
//
// this only redirects builtins 'puts' and '.c'. this does NOT redirect error messages
// and (builtin) prompts, they still go to the screen.
static void builtin_open_as_stdout(Interpreter *intr) {
	auto obj = intr->pop();
	if(obj.isVoid()) {
		if(fp_stdout!=stdout) {
			fclose(fp_stdout);
			fp_stdout = stdout;
		}
	}
	else if(obj.isString())
		fp_stdout = fopen(obj.asString(), "w");
	else
		throw LangError("Unknown arg to open-as-stdout: " + obj.fmtStackPrint());
}

#include "deserialize.hpp"

static void builtin_deserialize(Interpreter *intr) {
	auto filename = popString(intr,"deserialize");
	ifstream fileIn(filename);
	deserialize_stream(intr, fileIn);
	// no return, just loads words into interpreter
}

#include <stdio.h>

static void builtin_prompt(Interpreter *intr) {
	auto prompt = popString(intr,"prompt");
	// NOTE - ignore any user-set stdout since user needs to see prompt on screen
	printf("%s", prompt);
	fflush(stdout);
	char buf[256];
	cin.getline(buf, sizeof(buf));	
	if(cin.eof())
		intr->push(newVoid());
	else
		intr->push(newString(buf));
}	

#include <time.h>

static void builtin_time_string(Interpreter *intr) {
	char buf[100];
	time_t now;
	time(&now);
	strftime(buf,sizeof(buf)-1,"%Y-%m-%d %H:%M:%S",localtime(&now));
	intr->push(newString(buf));
}

// ( filename string -- )
// write string to filename, overwriting existing file
static void builtin_file_write(Interpreter *intr) {
	string text = popString(intr,"file-write");
	string filename = popString(intr,"file-write");
	FILE *fp = fopen(filename.c_str(), "w");
	if(!fp)
		throw LangError("Unable to open file for writing: " + filename);

	fputs(text.c_str(), fp);
	fclose(fp);
}

// ( filename string -- )
// append string to end of file (or start new file)
static void builtin_file_append(Interpreter *intr) {
	string text = popString(intr,"file-append");
	string filename = popString(intr,"file-append");
	FILE *fp = fopen(filename.c_str(), "a");
	if(!fp)
		throw LangError("Unable to open file for appending: " + filename);
		
	fputs(text.c_str(), fp);
	fclose(fp);
}

#include <cstdio>

// ( filename -- )
// delete file if it exists (no error if it does not exist)
static void builtin_file_delete(Interpreter *intr) {
	string filename = popString(intr,"file-append");
	remove(filename.c_str());
}

// NOT required to be sorted
static void builtin_keys(Interpreter *intr) {
	auto obj = popDict(intr, "keys");
	auto dict = obj.asDict();
	auto keys = newList();
	for(const auto& pair: *dict)
		keys.asList()->push_back(newString(pair.first));

	intr->push(keys);
}

std::map<std::string,BUILTIN_FUNC> BUILTINS { 
	{"+", [](Interpreter *intr) { do_binop(intr, &Object::opAdd); }},
	{"-", [](Interpreter *intr) { do_binop(intr, &Object::opSubtract); }},
	{"*", [](Interpreter *intr) { do_binop(intr, &Object::opMul); }},
	{"/", [](Interpreter *intr) { do_binop(intr, &Object::opDivide); }},
	{"/mod", builtin_divmod},
	{"f.setprec", [](Interpreter *intr) { FLOAT_PRECISION = popInt(intr, "Bad arg to f.setprec");}},
	{".c", builtin_printchar},
	{"puts", [](Interpreter *intr) 
		{fprintf(fp_stdout, "%s", popString(intr, "bad puts arg"));}},
	
	// - NOTE - repr & str COULD be implemented in verbii, however, they have to be
	//          implemented natively anyways for internal error printing, so
	//          no purpose in implementing twice

	// convert TOS to verbose printable string (like for stack display)
	{"repr", [](Interpreter *intr) {intr->push(newString(intr->pop().fmtStackPrint()));}},
	// convert TOS to normal printable string (like for '.')
	{"str", [](Interpreter *intr) {intr->push(newString(intr->pop().fmtDisplay()));}},
	{"==", [](Interpreter *intr) {pushBool(intr, intr->pop().opEqual(intr->pop()));}},
	{">", builtin_greater},
	
	{"int?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isInt()));}},
	{"float?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isFloat()));}},
	{"bool?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isBool()));}},
	{"null?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isNull()));}},
	{"void?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isVoid()));}},
	{"list?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isList()));}},
	{"string?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isString()));}},
	{"symbol?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isSymbol()));}},
	{"lambda?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isLambda()));}},
	{"closure?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isClosure()));}},

	{"length", [](Interpreter *intr) {intr->push(intr->pop().opLength());}},
	{"SP", [](Interpreter *intr){intr->push(newInt(intr->SP));}},
	{"SP!", builtin_setsp},
	{"LP", [](Interpreter *intr){intr->push(newInt(intr->LP));}},
	{"LP!", builtin_setlp},
	{">L", builtin_tolocal},
	{"L>", builtin_fromlocal},
	{"ref", builtin_ref},
	{"set!", builtin_set},
	{".wordlist", [](Interpreter *intr) {intr->push(intr->getWordlist());}},
	{".dumpword", builtin_dumpword},
	{"error", builtin_error},

	{"make-list", builtin_make_list},
	{"make-string", builtin_make_string},
	{"make-symbol", builtin_make_symbol},
	{"make-lambda", builtin_make_lambda},
	{"make-word", builtin_make_word},
	{"unmake", builtin_unmake},
	{"slice", builtin_slice},
	{"append", builtin_append},
	// could implement next two in script, however, host language has to have this
	// function anyways to deserialize programs, so just use that
	{"parse-int", [](Interpreter *intr){intr->push(parseInt(popStringOrSymbol(intr)));}},
	{"parse-float", [](Interpreter *intr){intr->push(parseFloat(popStringOrSymbol(intr)));}},
	{"void", [](Interpreter *intr){intr->push(newVoid());}},
	
	{".loadc", builtin_loadc},
	
	{"make-closure", builtin_make_closure},
	{"self", builtin_self_get},
	{"self!", builtin_self_set},
	{"put", builtin_put},
	{"get", builtin_get},
	{"deepcopy", builtin_deepcopy},
	{"alloc", builtin_alloc},
	{",,del", builtin_del},

	// bitops - defined with long names so user can pick their own shorthand
	{"bit-and", builtin_bit_and},
	{"bit-or", builtin_bit_or},
	{"bit-not", builtin_bit_not},
	{"bit-xor", builtin_bit_xor},
	{"bit-shr", builtin_bit_shr},
	{"bit-shl", builtin_bit_shl},

	{"run-time", builtin_run_time},
	{",,new-dict", builtin_new_dict},

	// new words needed for running boot.verb
	{"file-exists?", builtin_file_exists},
	{"file-mtime", builtin_file_mtime},
	{"open-as-stdout", builtin_open_as_stdout},
	{"deserialize", builtin_deserialize},
	{"prompt", builtin_prompt},
	{"set-exit-on-exception", [](Interpreter *intr){EXIT_ON_EXCEPTION = popBool(intr);}},
	{"set-allow-overwrite-words", [](Interpreter *intr){ALLOW_OVERWRITING_WORDS = popBool(intr);}},
	{"set-stacktrace-on-exception", [](Interpreter *intr){STACKTRACE_ON_EXCEPTION = popBool(intr);}},

	// more words added while making the random module
	{"time-string", builtin_time_string},
	// this is commonly defined as returning a float, but i'm defining it to return an int --
	// will make no difference in any math operation and this allows the result to be used
	// in an integer context
	{"floor", [](Interpreter *intr){intr->push(newInt((VINT)floor(popFloatOrInt(intr,"floor"))));}},

	// instead of introducing another language-specific object that has to be wrapped (FILE*, file handle, etc.),
	// the file I/O operations are defined atomically - they open the file, perform an action, and close the file.
	{"file-write", builtin_file_write},
	{"file-append", builtin_file_append},
	// what the above SHOULD be named ...
	{"file-read", builtin_file_read},
	{"file-delete", builtin_file_delete},

	{"sys-platform", [](Interpreter *intr){intr->push(newString(
		 string("g++ ") + to_string(__GNUC__) + "." + 
		 	to_string(__GNUC_MINOR__) + "." + to_string(__GNUC_PATCHLEVEL__)));}},

	{"depth", [](Interpreter *intr){intr->push(newInt(intr->SP_EMPTY - intr->SP));}},

	{"keys", builtin_keys},
};
