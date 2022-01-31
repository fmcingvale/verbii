/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "native.hpp"
#include "tagging.hpp"
#include "errors.hpp"
#include <cmath>
#include <string>
#include <vector>
#include <map>
#include <iostream>
using namespace std;

static int popInt(Interpreter *intr) {
	return taggedToInt(intr->pop());
}

static void pushInt(Interpreter *intr, int i) {
	intr->push(intToTagged(i));
}

static void pushBool(Interpreter *intr, bool b) {
	intr->push(boolToTagged(b));
}

static void builtin_add(Interpreter *intr) {
	pushInt(intr, popInt(intr) + popInt(intr));
}

static void builtin_subtract(Interpreter *intr) {
	int b = popInt(intr);
	int a = popInt(intr);
	pushInt(intr, a-b);
}

static void builtin_multiply(Interpreter *intr) {
	pushInt(intr, popInt(intr) * popInt(intr));
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
static int int_divmod(int a, int b, int *mod) {
	if(b == 0) {
		throw LangError("Divide by zero");
	}
	int quot = (int)floor(((double)(abs(a))) / ((double)(abs(b))));

	bool samesign = (a < 0 && b < 0) || (a >=0 && b >= 0);
	if(samesign) {
		*mod = a - quot*b;
		return quot;
	}
	else {
		*mod = a + quot*b;
		return -quot;
	}
}

static void builtin_divmod(Interpreter *intr) {
	int b = popInt(intr);
	int a = popInt(intr);
	int mod;
	int q = int_divmod(a,b,&mod);
	pushInt(intr, mod);
	pushInt(intr, q);
}

static void builtin_define_word(Interpreter *intr) {
	string name = intr->nextWordOrFail();
	Wordlist words;
	while(1) {
		string w = intr->nextWordOrFail();
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
		string w = intr->nextWordOrFail();
		if(w == ")") {
			return;
		}
	}
}

static void builtin_printchar(Interpreter *intr) {
	int c = popInt(intr);
	putc(c,stdout);
}

static void builtin_greater(Interpreter *intr) {
	int b = popInt(intr);
	int a = popInt(intr);
	
	pushBool(intr, a>b);
}

/* ( obj addr -- ) - save obj to addr */
static void builtin_set(Interpreter *intr) {
	int addr = popInt(intr);
	tagged t = intr->pop();
	if(addr < 0 || (unsigned int)addr >= intr->RAM.size()) {
		throw LangError("Bad address in set!: " + to_string(addr));
	}
	intr->RAM[addr] = t;
}

static void builtin_ref(Interpreter *intr) {
	int addr = popInt(intr);
	if(addr < 0 || (unsigned int)addr >= intr->RAM.size()) {
		throw LangError("Bad address in ref: " + to_string(addr));
	}
	intr->push(intr->RAM[addr]);
}

static void builtin_setsp(Interpreter *intr) {
	int addr = popInt(intr);
	if(addr < intr->SP_MIN || addr > intr->SP_EMPTY) {
		throw LangError("Bad address in SP!: " + to_string(addr));
	}
	intr->SP = addr;
}

static void builtin_setlp(Interpreter *intr) {
	int addr = popInt(intr);
	if(addr < intr->LP_MIN || addr > intr->LP_EMPTY) {
		throw LangError("Bad address in LP!: " + to_string(addr));
	}
	intr->LP = addr;
}

static void builtin_tolocal(Interpreter *intr) {
	if(intr->LP <= intr->LP_MIN) {
		throw LangError("Locals overflow");
	}
	intr->RAM[--intr->LP] = intr->pop();
}

static void builtin_fromlocal(Interpreter *intr) {
	if(intr->LP >= intr->LP_EMPTY) {
		throw LangError("Locals underflow");
	}
	intr->push(intr->RAM[intr->LP++]);
}

static void builtin_print_string(Interpreter *intr) {
	while(true) {
		auto word = intr->nextWordOrFail();
		if(word == "\"") {
			return; // end of string
		}
		else {
			cout << word << " ";
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
			// new wordlist ('lambda') does NOT exist in WORDS; it only exists
			// as a tagged value that must be called.
			tagged t = makeTaggedWordlist(wordlist);
			// put tagged value into wordlist, replacing the { ... }, so subsequent
			// calls will push the tagged value
			intr->reader.insertPrevWord(to_string(t.v));
			// the first time, I have to push the tagged value here
			intr->push(t);
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
	{"*", builtin_multiply},
	{"/mod", builtin_divmod},
	{":", builtin_define_word},
	// synonym for ':', for readability
	{"def", builtin_define_word},
	{"(", builtin_comment},
	{".c", builtin_printchar},
	{"repr", 
		[](Interpreter *intr) {printf("%s", reprTagged(intr->pop()).c_str());}},
	{"==", 
		[](Interpreter *intr) {pushBool(intr, popInt(intr) == popInt(intr));}},
	{">", builtin_greater},
	{"depth", 
		[](Interpreter *intr){intr->push(intToTagged(intr->SP_EMPTY - intr->SP));}},
	{"SP",
		[](Interpreter *intr){intr->push(intToTagged(intr->SP));}},
	{"SP!", builtin_setsp},
	{"LP",
		[](Interpreter *intr){intr->push(intToTagged(intr->LP));}},
	{"LP!", builtin_setlp},
	{">L", builtin_tolocal},
	{"L>", builtin_fromlocal},
	{"ref", builtin_ref},
	{"set!", builtin_set},
	{".\"", builtin_print_string},
	{".showdef", builtin_show_def},
	{"{", builtin_make_lambda},
};

