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

static void builtin_add(Interpreter *intr) {
	intr->push(intToTagged(taggedToInt(intr->pop())+taggedToInt(intr->pop())));
}

static void builtin_subtract(Interpreter *intr) {
	int b = taggedToInt(intr->pop());
	int a = taggedToInt(intr->pop());
	intr->push(intToTagged(a-b));
}

static void builtin_multiply(Interpreter *intr) {
	intr->push(intToTagged(taggedToInt(intr->pop())*taggedToInt(intr->pop())));
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

static void builtin_divide(Interpreter *intr) {
	int b = taggedToInt(intr->pop());
	int a = taggedToInt(intr->pop());
	int mod;
	intr->push(intToTagged(int_divmod(a,b,&mod)));
}

static void builtin_mod(Interpreter *intr) {
	int b = taggedToInt(intr->pop());
	int a = taggedToInt(intr->pop());
	int mod;
	int_divmod(a,b,&mod);
	intr->push(intToTagged(mod));
}

static void builtin_divmod(Interpreter *intr) {
	int b = taggedToInt(intr->pop());
	int a = taggedToInt(intr->pop());
	int mod;
	int q = int_divmod(a,b,&mod);
	intr->push(intToTagged(mod));
	intr->push(intToTagged(q));
}

static void builtin_define_word(Interpreter *intr) {
	string name = intr->reader.nextWord();
	Wordlist words;
	while(1) {
		string w = intr->reader.nextWord();
		if(w == "") {
			throw LangError("End of input while looking for ;");
		}
		else if(w == ";") {
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
		string w = intr->reader.nextWord();
		if(w == "") {
			throw LangError("End of input looking for )");
		}
		else if(w == ")") {
			return;
		}
	}
}

static void builtin_clear(Interpreter *intr) {
	intr->SP = intr->SP_EMPTY;
}

static void builtin_dot(Interpreter *intr) {
	printf("%s ", reprTagged(intr->pop()).c_str());
	fflush(stdout);
}

static void builtin_cr(Interpreter *intr) {
	printf("\n");
}

static void builtin_equal(Interpreter *intr) {
	intr->push(boolToTagged(taggedToInt(intr->pop()) == taggedToInt(intr->pop())));
}

static void builtin_greater(Interpreter *intr) {
	int b = taggedToInt(intr->pop());
	int a = taggedToInt(intr->pop());
	
	intr->push(boolToTagged(a>b));
}

std::map<std::string,BUILTIN_FUNC> BUILTINS { 
	{"+", builtin_add},
	{"-", builtin_subtract},
	{"*", builtin_multiply},
	{"/", builtin_divide},
	{"mod", builtin_mod},
	{"/mod", builtin_divmod},
	{":", builtin_define_word},
	// synonym for ':', for readability
	{"def", builtin_define_word},
	{"(", builtin_comment},
	{"clear", builtin_clear},
	{".", builtin_dot},
	{"CR", builtin_cr},
	{"==", builtin_equal},
	{">", builtin_greater},
};

