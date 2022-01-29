
/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "native.hpp"
#include "tagging.hpp"
#include <cmath>
#include <string>
#include <vector>
#include <map>
#include <iostream>
using namespace std;

static void native_add(Interpreter *intr) {
	int b = taggedToInt(intr->pop());
	int a = taggedToInt(intr->pop());
	intr->push(intToTagged(a+b));
}

static void native_subtract(Interpreter *intr) {
	int b = taggedToInt(intr->pop());
	int a = taggedToInt(intr->pop());
	intr->push(intToTagged(a-b));
}

static void native_multiply(Interpreter *intr) {
	int b = taggedToInt(intr->pop());
	int a = taggedToInt(intr->pop());
	intr->push(intToTagged(a*b));
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
static int int_divide(int a, int b) {
	int quot = (int)floor(((double)(abs(a))) / ((double)(abs(b))));

	bool samesign = (a < 0 && b < 0) || (a >=0 && b >= 0);
	if(samesign) {
		return quot;
	}
	else {
		return -quot;
	}
}

static void native_divide(Interpreter *intr) {
	int b = taggedToInt(intr->pop());
	int a = taggedToInt(intr->pop());
	intr->push(intToTagged(int_divide(a,b)));
}

static void native_define_word(Interpreter *intr) {
	string name = intr->nextWord();
	Wordlist words;
	while(1) {
		string w = intr->nextWord();
		if(w == "") {
			cout << "*** END OF INPUT WHILE LOOKING FOR ; ***\n";
			return;
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

std::map<std::string,BUILTIN_FUNC> BUILTINS { 
	{"+", native_add},
	{"-", native_subtract},
	{"*", native_multiply},
	{"/", native_divide},
	{":", native_define_word},
};

