/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

//
// TODO -- the functions that call intr->syntax-> should probably really be in Syntax
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

// TODO - move this to syntax layer and change WORDS to a linear list and
// convert words to index values -- add new 'call-word' in interpreter
//
// actually, could push as a lambda (or maybe a new class, since lambdas need an
// explicit call, and words don't) then interpreter doesn't need to know
// what WORDS is
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

static void builtin_show_def(Interpreter *intr) {
	auto name = intr->syntax->nextSymbolOrFail();
	auto word = intr->WORDS.find(name.asSymbol());
	if(word == intr->WORDS.end()) {
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

static void builtin_error(Interpreter *intr) {
	auto msg = intr->pop();
	if(!msg.isString()) {
		throw LangError("error expects string but got: " + msg.fmtStackPrint());
	}
	throw LangError(msg.asString());
}

static void do_binop(Interpreter *intr, Object (Object::*op)(const Object &)) {
	Object b = intr->pop();
	Object a = intr->pop();
	intr->push((a.*op)(b));
}

std::map<std::string,BUILTIN_FUNC> BUILTINS { 
	{"+", [](Interpreter *intr) { do_binop(intr, &Object::opAdd); }},
	{"-", [](Interpreter *intr) { do_binop(intr, &Object::opSubtract); }},
	{"*", [](Interpreter *intr) { do_binop(intr, &Object::opMul); }},
	{"/", [](Interpreter *intr) { do_binop(intr, &Object::opDivide); }},
	{"/mod", builtin_divmod},
	{"f.setprec",
		[](Interpreter *intr) { FLOAT_PRECISION = popInt(intr); }},
	{":", builtin_define_word},
	// synonym for ':', for readability
	{"def", builtin_define_word},
	{".c", builtin_printchar},
	{"puts", builtin_puts},
	// convert TOS to verbose printable string (like for stack display)
	{"repr", 
		[](Interpreter *intr) {intr->push(newString(intr->pop().fmtStackPrint()));}},
	// convert TOS to normal printable string (like for '.')
	{"str", 
		[](Interpreter *intr) {intr->push(newString(intr->pop().fmtDisplay()));}},
	{"==", 
		[](Interpreter *intr) {pushBool(intr, intr->pop().opEqual(intr->pop()));}},
	{">", 
		[](Interpreter *intr) {pushBool(intr, popFloatOrInt(intr) < popFloatOrInt(intr));}},
	{"int?",
		[](Interpreter *intr) {intr->push(newBool(intr->pop().isInt()));}},
	{"length", [](Interpreter *intr) {intr->push(intr->pop().opLength());}},
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
	{".showdef", builtin_show_def},
	{"error", builtin_error},
};
