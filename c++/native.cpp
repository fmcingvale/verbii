/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

//
// TODO -- the functions that call intr->syntax-> should probably really be in Syntax
//

// TODO -- might want 'dup' as builtin so it can optimize things like shallow copying strings
//         since they are immutable??
//

#include "native.hpp"
#include "errors.hpp"
#include "xmalloc.hpp"
#include <cmath>
#include <string>
#include <vector>
#include <map>
#include <iostream>
using namespace std;

Object native_cmdline_args;

static int popInt(Interpreter *intr, const char *errmsg) {
	Object obj = intr->pop();
	if(!obj.isInt()) {
		throw LangError(string(errmsg) + " (requires integer, got: " + obj.fmtStackPrint() + ")");
	}
	return obj.asInt();
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

static void pushInt(Interpreter *intr, int i) {
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
	int b = popInt(intr, "Bad divmod denominator");
	int a = popInt(intr, "Bad divmod numerator");
	int mod;

	if(b == 0) {
		throw LangError("Divide by zero");
	}
	int quot = (int)floor(((double)(abs(a))) / ((double)(abs(b))));

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

// TODO - move this to syntax layer and change WORDS to a linear list and
// convert words to index values -- add new 'call-word' in interpreter
//
// actually, could push as a lambda (or maybe a new class, since lambdas need an
// explicit call, and words don't) then interpreter doesn't need to know
// what WORDS is
#if 0
static void builtin_define_word(Interpreter *intr) {
	auto name = intr->syntax->nextSymbolOrFail();
	ObjList *objs = new ObjList();
	while(1) {
		auto o = intr->syntax->nextObjOrFail();
		if(o.isSymbol(";")) {
			intr->WORDS[name.asSymbol()] = objs;
			return;
		}
		else {
			objs->push_back(o);
		}
	}
}
#endif

/* ( list name -- adds word ) */
static void builtin_make_word(Interpreter *intr) {
	//cout << "MAKE-WORD ENTRY:" << intr->reprStack() << endl;
	const char *name = popSymbol(intr, "make-word bad name");
	Object list = popList(intr, "make-word bad list");
	intr->WORDS[name] = list.data.objlist;
}

static void builtin_printchar(Interpreter *intr) {
	int c = popInt(intr, "Bad printchar arg");
	putc(c, stdout);
	if(c == 10 || c == 13) {
		fflush(stdout);
	}
}

// ( obj addr -- ) - save obj to addr (index into OBJMEM)
static void builtin_set(Interpreter *intr) {
	Object addr = intr->pop();
	Object obj = intr->pop();
	if(addr.isInt()) {
		// SP or LP index
		int index = addr.asInt();
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
		int index = addr.asInt();
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
	int addr = popInt(intr, "Bad SP! arg");
	if(addr < intr->SP_MIN || addr > intr->SP_EMPTY) {
		throw LangError("Bad address in SP!: " + to_string(addr));
	}
	intr->SP = addr;
}

// set locals pointer from addr on stack
// (LP values must be integers)
static void builtin_setlp(Interpreter *intr) {
	int addr = popInt(intr, "Bad LP! arg");
	if(addr < intr->LP_MIN || addr > intr->LP_EMPTY) {
		throw LangError("Bad address in LP!: " + to_string(addr));
	}
	intr->LP = addr;
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

static void do_binop(Interpreter *intr, Object (Object::*op)(const Object &)) {
	Object b = intr->pop();
	Object a = intr->pop();
	intr->push((a.*op)(b));
}

// =============== experimental syntax scripting ===============================
#include <iostream>
#include <fstream>
static string readfile(string filename) {
	ifstream fileIn(filename);
	if(fileIn.rdstate() != ios_base::goodbit) {
		throw LangError("No such file in readfile():" + filename);
	}
	string line, buf;
	while(getline(fileIn, line)) {
		buf += "\n" + line;
	}
	return buf;
}

#include <sstream>
istringstream reader_input;
//Reader syntax_reader;
// read words from filename on top of stack, discarding any previous input
static void builtin_reader_open_file(Interpreter *intr) {
	const char *filename = popString(intr, "reader-open-file missing filename");
	string buf = readfile(filename);
	reader_input = istringstream(buf);
	//syntax_reader.clearAll();
	//syntax_reader.addText(buf);
}

// read words from string on top of stack, discarding any previous input
static void builtin_reader_open_string(Interpreter *intr) {
	const char *text = popString(intr, "reader-open-string missing text");
	//syntax_reader.clearAll();
	//syntax_reader.addText(text);
	reader_input = istringstream(text);
}

static void builtin_reader_next(Interpreter *intr) {
	string word;
	reader_input >> word;
	if(reader_input.fail()) {
		intr->push(newNull());
	}
	else {
		intr->push(newSymbol(word));
	}
	//Object sym = syntax_reader.nextObj();
	//cout << "builtin-reader-next:" << sym.fmtStackPrint() << endl;
	//if(!sym.isNull() && !sym.isSymbol()) {
	//	throw LangError("reader-next expecting null or symbol but got: " + sym.fmtStackPrint());
	//}
	//intr->push(sym);
}

// ( sn .. s1 N -- list of N items; N can be zero for an empty list )
static void builtin_make_list(Interpreter *intr) {
	Object list = newList();
	int nr = popInt(intr, "Bad make-list number of items");
	for(int i=0; i<nr; ++i) {
		list.data.objlist->insert(list.data.objlist->begin(), intr->pop());
	}
	intr->push(list);
}

static void builtin_slice(Interpreter *intr) {
	int nr = popInt(intr, "Bad slice count");
	int index = popInt(intr, "Bad slice index" );
	Object obj = intr->pop();
	intr->push(obj.opSlice(index, nr));
}

static void builtin_append(Interpreter *intr) {
	Object add = intr->pop();
	Object list = popList(intr, "Bad arg to append");
	list.data.objlist->push_back(add);
	intr->push(list);
}

static void builtin_make_lambda(Interpreter *intr) {
	Object list = popList(intr, "Bad arg to make-lambda");
	intr->push(newLambda(list.data.objlist));
}

static void builtin_greater(Interpreter *intr) {
	Object b = intr->pop();
	Object a = intr->pop();
	pushBool(intr, a.opGreater(b));
}

static const char *popStringOrSymbol(Interpreter *intr) {
	Object o = intr->pop();
	if(o.isString()) {
		return o.asString();
	}
	else if(o.isSymbol()) {
		return o.asSymbol();
	}
	else {
		throw LangError("Expecting string or symbol but got: " + o.fmtStackPrint());
	}
}

static void builtin_unmake(Interpreter *intr) {
	Object obj = intr->pop();
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
		// turn lambda back into a list so make-lambda would work.
		//
		// would be nice to just change type to TYPE_LIST, but probably best
		// to create new list to avoid side effects ...
		Object outlist = newList();
		for(auto obj : *obj.data.objlist) {
			outlist.data.objlist->push_back(obj);
		}
		intr->push(outlist);
	}
	else
		throw LangError("Object cannot be unmade: " + obj.fmtStackPrint());
}

/* ( cn .. c1 N -- string of N chars ) */
static void builtin_make_string(Interpreter *intr) {
	int nr = popInt(intr, "Bad count in make-string");
	string s = "";
	for(int i=0; i<nr; ++i) {
		s.insert(s.begin(),(char)popInt(intr, "Bad char in make-string"));
	}
	intr->push(newString(s));
}

/* ( cn .. c1 N -- symbol of N chars ) */
static void builtin_make_symbol(Interpreter *intr) {
	int nr = popInt(intr, "Bad count in make-symbol");
	string s = "";
	for(int i=0; i<nr; ++i) {
		s.insert(s.begin(),(char)popInt(intr, "Bad char in make-symbol"));
	}
	intr->push(newSymbol(s));
}

static void builtin_wordlist(Interpreter *intr) {
	Object list = newList();
	for(const auto& pair : intr->WORDS) {
		list.data.objlist->push_back(newSymbol(pair.first));
	}
	intr->push(list);
}

static void builtin_varlist(Interpreter *intr) {
	Object list = newList();
	for(const auto& pair : intr->VARS) {
		list.data.objlist->push_back(newSymbol(pair.first));
	}
	intr->push(list);
}

static void builtin_dumpword(Interpreter *intr) {
	const char* symbol = popSymbol(intr,"Bad name in .dumpword");
	ObjList *wordlist = intr->lookup_word(symbol);
	if(!wordlist) {
		throw LangError("No such word in .dumpword: " + string(symbol));
	}
	intr->push(newList(wordlist));
}

#include "deserialize.hpp"

static void builtin_loadc(Interpreter *intr) {
	const char *filename = popString(intr, "Bad filename for .loadc");
	ifstream fileIn(filename);
	Object o = deserialize_stream(intr, fileIn);
	//intr->push(o);
}

static void builtin_cmdline_args(Interpreter *intr) {
	intr->push(native_cmdline_args);
}

std::map<std::string,BUILTIN_FUNC> BUILTINS { 
	{"+", [](Interpreter *intr) { do_binop(intr, &Object::opAdd); }},
	{"-", [](Interpreter *intr) { do_binop(intr, &Object::opSubtract); }},
	{"*", [](Interpreter *intr) { do_binop(intr, &Object::opMul); }},
	{"/", [](Interpreter *intr) { do_binop(intr, &Object::opDivide); }},
	{"/mod", builtin_divmod},
	{"f.setprec", [](Interpreter *intr) { FLOAT_PRECISION = popInt(intr, "Bad arg to f.setprec");}},
	// TODO - implement in script
	//{":", builtin_define_word},
	// synonym for ':', for readability (TODO implement in script)
	//{"def", builtin_define_word},
	{".c", builtin_printchar},
	{"puts", [](Interpreter *intr) {printf("%s", popString(intr, "bad puts arg"));}},
	
	// - NOTE - repr & str COULD be implemented in script, however, they have to be
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
	{"list?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isList()));}},
	{"string?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isString()));}},
	{"symbol?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isSymbol()));}},
	{"lambda?", [](Interpreter *intr) {intr->push(newBool(intr->pop().isLambda()));}},

	{"length", [](Interpreter *intr) {intr->push(intr->pop().opLength());}},
	{"SP", [](Interpreter *intr){intr->push(newInt(intr->SP));}},
	{"SP!", builtin_setsp},
	{"LP", [](Interpreter *intr){intr->push(newInt(intr->LP));}},
	{"LP!", builtin_setlp},
	{">L", builtin_tolocal},
	{"L>", builtin_fromlocal},
	{"ref", builtin_ref},
	{"set!", builtin_set},
	{".wordlist", builtin_wordlist},
	{".varlist", builtin_varlist},
	{".dumpword", builtin_dumpword},
	{"error", builtin_error},

	// experimental
	{"reader-open-file", builtin_reader_open_file},
	{"reader-open-string", builtin_reader_open_string},
	{"reader-next", builtin_reader_next},
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
	{"null", [](Interpreter *intr){intr->push(Object());}},
	{".loadc", builtin_loadc},
	{"cmdline-args", builtin_cmdline_args},
};
