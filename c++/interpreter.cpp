/*
	Interpreter - runs code.

	There is no compilation step, not really even a parsing step -- the interpreter
	runs directly from the wordlists from the Reader. This makes the code smaller and
	makes e.g. forward declarations really easy since nothing is evaluated until it
	runs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include <iostream>
#include "interpreter.hpp"
#include "native.hpp"
#include "errors.hpp"
#include "xmalloc.hpp"
using namespace std;

map<string,Wordlist> WORDS;
vector<Wordlist*> LAMBDAS;
map<string,Object> VARS;

Interpreter::Interpreter() {

	SIZE_STACKLOCALS = STACK_SIZE+LOCALS_SIZE;
	STACKLOCALS = (Object*)x_malloc(SIZE_STACKLOCALS*sizeof(Object));

	// stack starts at top grows downward

	// the _EMPTY indexes are not valid storage locations, they indicated the
	// respective stacks are emtpy
	SP_EMPTY = SIZE_STACKLOCALS;
	SP = SP_EMPTY;
	SP_MIN = SP_EMPTY - STACK_SIZE;

	LP_EMPTY = SP_MIN;
	LP = LP_EMPTY;
	LP_MIN = LP_EMPTY - LOCALS_SIZE;
	// sanity that I did that math correctly ...
	if(LP_MIN != 0) {
		throw LangError("stacklocals size is wrong!");
	}
}

void Interpreter::addText(const string &text) {
	reader.addText(text);
}

void Interpreter::push(Object obj) {
	if(SP <= SP_MIN) {
		throw LangError("Stack overflow");
	}
	STACKLOCALS[--SP] = obj;
}

Object Interpreter::pop() {
	if(SP >= SP_EMPTY) {
		throw LangError("Stack underflow");
	}
	return STACKLOCALS[SP++];
}

string Interpreter::reprStack() const {
	string s = "";
	for(int i=SP_EMPTY-1; i>=SP; --i) {
		s += STACKLOCALS[i].repr() + " ";
	}
	return s;
}

// take word like '>>NAME' or '<<NAME' and jump to '@NAME'
void Interpreter::do_jump(const string &jumpword) {
	//cout << "DO_JUMP TO: " << jumpword << endl;
	if(jumpword.substr(0,2) == ">>") {
		// forward jump, find word (>>NAME -> @NAME)
		while(true) {
			auto word = nextWordOrFail();
			//cout << "NEXT-WORD: " << word << endl;
			if(word.substr(1) == jumpword.substr(2)) {
				//cout << "FOUND" << endl;
				return; // found word, stop
			}
		}
	}
	else if(jumpword.substr(0,2) == "<<") {
		// backward jump
		while(true) {
			auto word = prevWordOrFail();
			//cout << "PREV-WORD: " << word << endl;
			if(word.substr(1) == jumpword.substr(2)) {
				//cout << "FOUND" << endl;
				return; // found word, stop
			}
		}
	}
	else {
		throw LangError("Bad jumpword " + jumpword);
	}
}

const string& Interpreter::nextWordOrFail() {
	if(reader.peekWord() == "") {
		throw LangError("Unexpected end of input");
	}
	return reader.nextWord();
}

const string& Interpreter::prevWordOrFail() {
	if(reader.peekPrevWord() == "") {
		throw LangError("Unable to find previous word");
	}
	return reader.prevWord();
}

void Interpreter::run(bool singlestep) {
	// run one word at a time in a loop, with the reader position as the continuation
	while(true) {
		auto word = reader.nextWord();
		// general note: there is kind of an artificial separation between words that are
		// recognized here, and words implemented in native.cpp. In priciple, all these words
		// could be implemented in native.cpp. However, I tend to put words here that directly
		// affect the interpreter inner state vs. functions that operate on data that go into
		// native.cpp. however, that's not always the case. basically, there's nothing magical
		// about why a word is here vs in native.cpp.
		// another consideration is how often words run -- better to have the most commonly run
		// words here since 'if(word == ...)' is a lot faster than a map lookup. But .. again ..
		// no hard and fast rule about it.
		if(word == "") {
			// i could be returning from a word that had no 'return',
			// so pop words like i would if it were a return
			if(reader.hasPushedWords()) {
				reader.popWords();
				continue;
			}
			else {
				return;
			}
		}
		if(singlestep) {
			cout << "Run word: " << word << endl;
			cout << "=> " << reprStack() << endl;
			string line;
			getline(cin, line);
		}
		
		// NOTE - i was originally using c++ regexes here to check for
		// integers and lambdas. after profiling and discovering they
		// were the using most of the program runtime, i changed to parsing
		// them myself. WITH regexes, runtime was 4.5x higher and memory
		// usage was 1500x (!!) larger (see commit [556839e] for the regex version)
		{
			// see if it's an integer
			bool has_digits = false;
			const char *s = word.c_str();
			if(*s == '+' || *s == '-') 
				++s;

			while(isdigit(*s)) {
				has_digits = true;
				++s;
			}

			// integers are just pushed to stack
			if(!*s && has_digits) {
				//cout << "MATCHED INT:" << word << endl;
				push(newInt(stoi(word)));
				continue;
			}
		}

		{
			// to keep parsing simple (i.e. should be easy to do without regexes),
			// floats are written like #n.nnn
			if(word[0] == '#') {
				push(newFloat(stod(word.c_str()+1)));
				continue;
			}
		}

		{
			// check for $<lambda NN>
			if(!strncmp(word.c_str(), "$<lambda ", 9)) {
				const char *s = word.c_str() + 9;
				int num = 0;
				while(isdigit(*s)) {
					num = num*10 + *s - '0';
					++s;
				}
				push(newLambda(num));
				continue;
			}
		}

		if(word == "return") {
			// return from word by popping back to previous wordlist (if not at toplevel)
			if(reader.hasPushedWords()) {	
				reader.popWords();
			}
			else {
				return; // return from top level exits program
			}
			continue;
		}

		if(word == "if") {
			// true jump is required
			auto true_jump = reader.nextWord();
			//cout << "TRUE_JUMP: " << true_jump << endl;
			// false word is optional
			bool have_false_jump = false;
			if(reader.peekWord().substr(0,2) == "<<" || reader.peekWord().substr(0,2) == ">>") {
				have_false_jump = true;
			}
			//cout << "FALSE_JUMP: " << false_jump << endl;
			Object cond = pop();
			//cout << "POPPED COND: " << cond.repr() << endl;
			if(!cond.isBool()) {
				throw LangError("'if' requires true|false but got: " + cond.repr());
			}
			// these don't run the jump, they just reposition the reader
			if(cond.asBool()) {
				// no need to actually skip false jump since i'll be looking for '@'
				do_jump(true_jump);
			}
			else if(have_false_jump) {
				const string& false_jump = reader.nextWord();
				do_jump(false_jump);
			}
			continue;
		}

		if(word.substr(0,2) == ">>" || word.substr(0,2) == "<<") {
			do_jump(word);
			continue;
		}

		if(word.substr(0,1) == "@") {
			// jump target -- ignore
			continue;
		}

		if(word == "var") {
			auto name = nextWordOrFail();
			auto count = stoi(nextWordOrFail());
			// must be unique userword
			if(VARS.find(name) != VARS.end()) {
				throw LangError("Trying to redefine variable " + name);
			}
			// add to VARS so name lookup works (below)
			VARS[name] = newMemArray(count, 0);
			continue;
		}

		if(word == "del") {
			auto name = nextWordOrFail();
			auto userword = VARS.find(name);
			if(userword == VARS.end()) {
				throw LangError("Trying to delete non-existent variable " + name);
			}
			VARS.erase(name);
			continue;
		}

		if(word == "call") {
			// top of stack must be a lambda
			auto val = pop();
			if(!val.isLambda()) {
				throw LangError("call expects a lambda, but got: " + val.repr());
			}
			// now this is just like calling a userword, below
			// TODO -- tail call elimination??
			reader.pushWords(LAMBDAS[val.asLambdaIndex()]);
			continue;
		}

		// builtins, then userwords, then vars

		auto bltin = BUILTINS.find(word);
		if(bltin != BUILTINS.end()) {
			bltin->second(this);
			continue;
		}
		
		auto userword = WORDS.find(word);
		if(userword != WORDS.end()) {
			// tail call elimination -- if end of this wordlist OR next word is 'return', then
			// i don't need to come back here, so pop my wordlist first to stop stack from growing
			if(reader.peekWord() == "" || reader.peekWord() == "return") {
				if(reader.hasPushedWords()) { // in case i'm at the toplevel
					reader.popWords();
				}
			}
			// execute word by pushing its wordlist and continuing
			reader.pushWords(&userword->second);
			continue;
		}

		auto var = VARS.find(word);
		if(var != VARS.end()) {
			push(var->second);
			continue;
		}

		throw LangError(string("Unknown word ") + word);
	}
}
