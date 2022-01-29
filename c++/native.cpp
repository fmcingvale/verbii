
/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "native.hpp"
#include <cmath>
#include <string>
#include <vector>
#include <map>
#include <iostream>
using namespace std;

static void native_add(Interpreter *intr) {
	intr->push(intr->pop()+intr->pop());
}

static void native_subtract(Interpreter *intr) {
	int b = intr->pop();
	int a = intr->pop();
	intr->push(a-b);
}

static void native_multiply(Interpreter *intr) {
	intr->push(intr->pop()*intr->pop());
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

static void native_divide(Interpreter *intr) {
	int b = intr->pop();
	int a = intr->pop();
	int mod;
	intr->push(int_divmod(a,b,&mod));
}

static void native_mod(Interpreter *intr) {
	int b = intr->pop();
	int a = intr->pop();
	int mod;
	int_divmod(a,b,&mod);
	intr->push(mod);
}

static void native_divmod(Interpreter *intr) {
	int b = intr->pop();
	int a = intr->pop();
	int mod;
	int q = int_divmod(a,b,&mod);
	intr->push(mod);
	intr->push(q);
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

static void native_comment(Interpreter *intr) {
	while(1) {
		string w = intr->nextWord();
		if(w == "") {
			cout << "*** End of input looking for ) ***\n";
		}
		else if(w == ")") {
			return;
		}
	}
}

static void native_clear(Interpreter *intr) {
	intr->SP = intr->SP_EMPTY;
}

static void native_dot(Interpreter *intr) {
	printf("%d ", intr->pop());
	fflush(stdout);
}

static void native_cr(Interpreter *intr) {
	printf("\n");
}

std::map<std::string,BUILTIN_FUNC> BUILTINS { 
	{"+", native_add},
	{"-", native_subtract},
	{"*", native_multiply},
	{"/", native_divide},
	{"mod", native_mod},
	{"/mod", native_divmod},
	{":", native_define_word},
	// synonym for ':', for readability
	{"def", native_define_word},
	{"(", native_comment},
	{"clear", native_clear},
	{".", native_dot},
	{"CR", native_cr},
};

