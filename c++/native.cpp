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
using namespace std;

static int popInt(Interpreter *intr) {
	Object obj = intr->pop();
	if(!obj.isInt()) {
		throw LangError("Expecting int but got: " + obj.fmtStackPrint());
	}
	return obj.asInt();
}

static void pushInt(Interpreter *intr, int i) {
	intr->push(newInt(i));
}

static void pushBool(Interpreter *intr, bool b) {
	intr->push(newBool(b));
}

static double popFloatOrInt(Interpreter *intr) {
	Object obj = intr->pop();
	if(obj.isFloat()) {
		return obj.data.d;
	}
	else if(obj.isInt()) {
		return obj.data.i;
	}
	else {
		throw LangError("Expecting int or float but got: " + obj.fmtStackPrint());
	}
}

static void pushFloat(Interpreter *intr, double d) {
	intr->push(newFloat(d));
}

static void builtin_add_float(Interpreter *intr) {
	double b = popFloatOrInt(intr);
	double a = popFloatOrInt(intr);
	pushFloat(intr, a+b);
}

static void builtin_sub_float(Interpreter *intr) {
	double b = popFloatOrInt(intr);
	double a = popFloatOrInt(intr);
	pushFloat(intr, a-b);
}

static void builtin_mul_float(Interpreter *intr) {
	double b = popFloatOrInt(intr);
	double a = popFloatOrInt(intr);
	pushFloat(intr, a*b);
}

static void builtin_div_float(Interpreter *intr) {
	double b = popFloatOrInt(intr);
	if(b == 0) {
		throw LangError("Floating point divide by zero");
	}
	double a = popFloatOrInt(intr);
	pushFloat(intr, a/b);
}

static void builtin_add(Interpreter *intr) {
	Object b = intr->pop();
	Object a = intr->pop();
	if(a.isInt() && b.isInt()) {
		// since a was popped by value, safe to overwrite 
		// instead of making new object
		a.setInt(a.asInt() + b.asInt());
		intr->push(a);
	}
	else if(a.isMemArray() && b.isInt()) {
		// can't directly reuse 'a' since offset and count are part of
		// the allocated object, but can share underlying array
		Object r = copyMemArray(a.asMemArray());
		r.asMemArray()->offset += b.asInt();
		intr->push(r);
	}
	else if(a.isInt() && b.isMemArray()) {
		// swapped version of above
		Object r = copyMemArray(b.asMemArray());
		r.asMemArray()->offset += a.asInt();
		intr->push(r);
	}
	else {
		throw LangError("Can't add values: " + a.fmtStackPrint() + " + " + b.fmtStackPrint());
	}
}

// i think only '+' makes sense for MemoryArray, so not implementing -
static void builtin_subtract(Interpreter *intr) {
	int b = popInt(intr);
	int a = popInt(intr);
	pushInt(intr, a-b);
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
	int b = popInt(intr);
	int a = popInt(intr);
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

static void builtin_define_word(Interpreter *intr) {
	auto name = intr->nextSymbolOrFail();
	ObjList *objs = new ObjList();
	while(1) {
		auto o = intr->nextObjOrFail();
		if(o.isSymbol(";")) {
			WORDS[name.asSymbol()] = objs;
			return;
		}
		else {
			objs->push_back(o);
		}
	}
}

static void builtin_comment(Interpreter *intr) {
	while(1) {
		auto o = intr->nextObjOrFail();
		if(o.isSymbol(")")) {
			return;
		}
	}
}

static void builtin_printchar(Interpreter *intr) {
	int c = popInt(intr);
	putc(c, stdout);
	if(c == 10 || c == 13) {
		fflush(stdout);
	}
}

static void builtin_puts(Interpreter *intr) {
	auto obj = intr->pop();
	if(!obj.isString()) {
		throw LangError("puts requires string but got: " + obj.fmtStackPrint());
	}
	printf("%s", obj.asString());
}

// ( obj addr -- ) - save obj to addr
//
// two cases:
//	* addr is integer == index into STACKLOCALS
//	* addr is MemoryArray
static void builtin_set(Interpreter *intr) {
	Object addr = intr->pop();
	Object obj = intr->pop();
	if(addr.isInt()) {
		// SP or LP index
		int index = addr.asInt();
		if(index < 0 || index > intr->SIZE_STACKLOCALS) {
			throw LangError("Bad address in set!: " + to_string(index));
		}
		intr->STACKLOCALS[index] = obj;
	}
	else if(addr.isMemArray()) {
		auto arr = addr.asMemArray();
		if(arr->offset < 0 || arr->offset >= arr->count) {
			throw LangError("Offset out of bounds in set!");
		}
		arr->array[arr->offset] = obj;
	}
	else {
		throw LangError("NOT IMPLEMENTED IN set!");
	}
}

// ( addr -- obj ) load obj from addr and push to stack
//
// as above, addr can be int or MemoryArray
static void builtin_ref(Interpreter *intr) {
	Object addr = intr->pop();
	if(addr.isInt()) {
		int index = addr.asInt();
		if(index < 0 || index >= intr->SIZE_STACKLOCALS) {
			throw LangError("Bad address in ref: " + to_string(index));
		}
		intr->push(intr->STACKLOCALS[index]);
	}
	else if(addr.isMemArray()) {
		auto arr = addr.asMemArray();
		if(arr->offset < 0 || arr->offset >= arr->count) {
			throw LangError("Offset out of bounds in ref");
		}
		intr->push(arr->array[arr->offset]);
	}
	else {
		throw LangError("NOT IMPLEMENTED IN ref");
	}
}

// set stack pointer from addr on stack
// (SP values must be integers)
static void builtin_setsp(Interpreter *intr) {
	int addr = popInt(intr);
	if(addr < intr->SP_MIN || addr > intr->SP_EMPTY) {
		throw LangError("Bad address in SP!: " + to_string(addr));
	}
	intr->SP = addr;
}

// set locals pointer from addr on stack
// (LP values must be integers)
static void builtin_setlp(Interpreter *intr) {
	int addr = popInt(intr);
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
	intr->STACKLOCALS[--intr->LP] = intr->pop();
}

// pop top locals and push to stack
static void builtin_fromlocal(Interpreter *intr) {
	if(intr->LP >= intr->LP_EMPTY) {
		throw LangError("Locals underflow");
	}
	intr->push(intr->STACKLOCALS[intr->LP++]);
}

// ." some string here " -- print string
static void builtin_print_string(Interpreter *intr) {
	while(true) {
		auto obj = intr->nextObjOrFail();
		if(obj.isSymbol("\"")) {
			return; // end of string
		}
		else {
			printf("%s ", obj.asSymbol());
		}
	}
}

static void builtin_show_def(Interpreter *intr) {
	auto name = intr->nextSymbolOrFail();
	auto word = WORDS.find(name.asSymbol());
	if(word == WORDS.end()) {
		cout << "No such word: " << name.fmtStackPrint() << endl;
		return;
	}
	auto objlist = word->second;
	cout << name.fmtStackPrint() << ": ";
	for(auto o : *objlist) {
		cout << o.fmtStackPrint() << " ";
	}
	cout << ";\n";
}

static void builtin_make_lambda(Interpreter *intr) {
	// turn { ... } into an anonymous wordlist
	//
	// the FIRST time I see { .. }, create a tagged wordlist,
	// REWRITE the { ... } into the tagged value and push the tagged
	// value to the stack.
	//
	// each SUBSEQUENT time the same code runs, the tagged value will be
	// in the wordlist, so will be pushed to the stack.
	//
	// from the perspective of the user, the same thing happened both times --
	// the tagged lambda was pushed to the stack, ready to be called, stored, etc.
	
	// { was just read -- read the words until }
	
	// delete the { that was just read
	intr->reader.deletePrevObj();

	auto objlist = new ObjList();
	int nesting = 1;
	while(true) {
		auto obj = intr->nextObjOrFail();
		// delete the { ... } as I read it -- will replace it with a tagged wordlist
		intr->reader.deletePrevObj();
		
		if(obj.isSymbol("{")) {
			// if I find inner lambdas, just copy them for now and later when they are run, 
			// this same process will happen for them
			++nesting;
			objlist->push_back(obj);
		}
		else if(obj.isSymbol("}")) {
			if(--nesting > 0) {
				objlist->push_back(obj);
				continue;
			}
			// new unnamed wordlist will be placed into LAMBDAS, and its index placed
			// on stack and in source wordlist so a subsequent 'call' can find it
			LAMBDAS.push_back(objlist);
			int index = LAMBDAS.size()-1;

			// XXX FIX ME -- just push Lambda object directly, don't even need LAMBDAS anymore

			// replace { .. } in source wordlist with "$<lambda index>" so subsequent 'call'
			// can find it (note it would be impossible for user code to insert this word
			// from source since it contains whitespace)
			intr->reader.insertPrevObj(newSymbol("$<lambda " + to_string(index) + ">"));
			// the first time, I have to push the lambda object as well -- interpreter
			// will do this on subsequent calls when it sees "lambda<#>"
			intr->push(newLambda(index));
			return;
		}
		else {
			objlist->push_back(obj);
		}
	}
}

std::map<std::string,BUILTIN_FUNC> BUILTINS { 
	{"+", builtin_add},
	{"-", builtin_subtract},
	{"*", 
		[](Interpreter *intr) {pushInt(intr, popInt(intr) * popInt(intr));}},
	{"/mod", builtin_divmod},
	{"f+", builtin_add_float},
	{"f-", builtin_sub_float},
	{"f*", builtin_mul_float},
	{"f/", builtin_div_float},
	{"f.setprec",
		[](Interpreter *intr) { FLOAT_PRECISION = popInt(intr); }},
	{":", builtin_define_word},
	// synonym for ':', for readability
	{"def", builtin_define_word},
	{"(", builtin_comment},
	{".c", builtin_printchar},
	{"puts", builtin_puts},
	// convert TOS to verbose printable string (like for stack display)
	{"repr", 
		[](Interpreter *intr) {intr->push(newString(intr->pop().fmtStackPrint()));}},
	// convert TOS to normal printable string (like for '.')
	{"str", 
		[](Interpreter *intr) {intr->push(newString(intr->pop().fmtDisplay()));}},
	{"==", 
		[](Interpreter *intr) {pushBool(intr, popFloatOrInt(intr) == popFloatOrInt(intr));}},
	{">", 
		[](Interpreter *intr) {pushBool(intr, popFloatOrInt(intr) < popFloatOrInt(intr));}},
	{"int?",
		[](Interpreter *intr) {intr->push(newBool(intr->pop().isInt()));}},
	{"depth", 
		[](Interpreter *intr){intr->push(newInt(intr->SP_EMPTY - intr->SP));}},
	{"SP",
		[](Interpreter *intr){intr->push(newInt(intr->SP));}},
	{"SP!", builtin_setsp},
	{"LP",
		[](Interpreter *intr){intr->push(newInt(intr->LP));}},
	{"LP!", builtin_setlp},
	{">L", builtin_tolocal},
	{"L>", builtin_fromlocal},
	{"ref", builtin_ref},
	{"set!", builtin_set},
	{".\"", builtin_print_string},
	{".showdef", builtin_show_def},
	{"{", builtin_make_lambda},
};
