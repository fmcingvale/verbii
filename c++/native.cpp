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
		throw LangError("Expecting int but got: " + obj.repr());
	}
	return obj.asInt();
}

static void pushInt(Interpreter *intr, int i) {
	intr->push(newInt(i));
}

static void pushBool(Interpreter *intr, bool b) {
	intr->push(newBool(b));
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
	else {
		throw LangError("Can't add values: " + a.repr() + " + " + b.repr());
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
	auto name = intr->nextWordOrFail();
	Wordlist words;
	while(1) {
		auto w = intr->nextWordOrFail();
		if(w == ";") {
			WORDS[name] = words;
			return;
		}
		else {
			words.push_back(w);
		}
	}
}

static void builtin_comment(Interpreter *intr) {
	while(1) {
		auto w = intr->nextWordOrFail();
		if(w == ")") {
			return;
		}
	}
}

static void builtin_printchar(Interpreter *intr) {
	int c = popInt(intr);
	printf("%c", c);
	if(c == 10 || c == 13) {
		fflush(stdout);
	}
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
			throw LangError("Offset out of bounds in set!");
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
		auto word = intr->nextWordOrFail();
		if(word == "\"") {
			return; // end of string
		}
		else {
			printf("%s ", word.c_str());
		}
	}
}

static void builtin_show_def(Interpreter *intr) {
	auto name = intr->nextWordOrFail();
	auto word = WORDS.find(name);
	if(word == WORDS.end()) {
		cout << "No such word: " << name << endl;
		return;
	}
	auto wordlist = word->second;
	cout << name << ": ";
	for(auto w : wordlist) {
		cout << w << " ";
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
	intr->reader.deletePrevWord();

	// TODO -- need to unify handling/storage of wordlists - right now WORDS holds
	// Wordlists by value and tagging is done by pointer.
	auto wordlist = new Wordlist();
	int nesting = 1;
	while(true) {
		auto word = intr->nextWordOrFail();
		// delete the { ... } as I read it -- will replace it with a tagged wordlist
		intr->reader.deletePrevWord();
		if(word == "{") {
			// if I find inner lambdas, just copy them for now and later when they are run, 
			// this same process will happen for them
			++nesting;
			wordlist->push_back(word);
		}
		else if(word == "}") {
			if(--nesting > 0) {
				wordlist->push_back(word);
				continue;
			}
			// new unnamed wordlist will be placed into LAMBDAS, and its index placed
			// on stack and in source wordlist so a subsequent 'call' can find it
			LAMBDAS.push_back(wordlist);
			int index = LAMBDAS.size()-1;

			// replace { .. } in source wordlist with "$<lambda index>" so subsequent 'call'
			// can find it (note it would be impossible for user code to insert this word
			// from source since it contains whitespace)
			intr->reader.insertPrevWord("$<lambda " + to_string(index) + ">");
			// the first time, I have to push the lambda object as well -- interpreter
			// will do this on subsequent calls when it sees "lambda<#>"
			intr->push(newLambda(index));
			return;
		}
		else {
			wordlist->push_back(word);
		}
	}
}

std::map<std::string,BUILTIN_FUNC> BUILTINS { 
	{"+", builtin_add},
	{"-", builtin_subtract},
	{"*", 
		[](Interpreter *intr) {pushInt(intr, popInt(intr) * popInt(intr));}},
	{"/mod", builtin_divmod},
	{":", builtin_define_word},
	// synonym for ':', for readability
	{"def", builtin_define_word},
	{"(", builtin_comment},
	{".c", builtin_printchar},
	{"repr", 
		[](Interpreter *intr) {printf("%s", intr->pop().repr().c_str());}},
	{"==", 
		[](Interpreter *intr) {pushBool(intr, popInt(intr) == popInt(intr));}},
	{">", 
		[](Interpreter *intr) {pushBool(intr, popInt(intr) < popInt(intr));}},
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
