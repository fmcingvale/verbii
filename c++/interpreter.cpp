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
using namespace std;

map<string,Wordlist> WORDS;
vector<Wordlist*> LAMBDAS;

Interpreter::Interpreter() {
	// stack starts at top of memory and grows downward
	SP_EMPTY = RAM_SIZE - 1;
	SP = SP_EMPTY;
	SP_MIN = SP_EMPTY - STACK_SIZE;

	LP_EMPTY = SP_MIN;
	LP = LP_EMPTY;
	LP_MIN = LP_EMPTY - LOCALS_SIZE;

	MEM_LAST = LP_MIN - 1;
	MEM_NEXT = 0;

	re_integer = new regex(R"""(^[+\-]?[0-9]+$)""");
	re_lambda = new regex(R"""(\$<lambda ([0-9]+)>)""");
}

void Interpreter::addText(const string &text) {
	reader.addText(text);
}

void Interpreter::push(Object obj) {
	if(SP <= SP_MIN) {
		throw LangError("Stack overflow");
	}
	RAM[--SP] = obj;
}

Object Interpreter::pop() {
	if(SP >= SP_EMPTY) {
		throw LangError("Stack underflow");
	}
	return RAM[SP++];
}

string Interpreter::reprStack() const {
	string s = "";
	for(int i=SP_EMPTY-1; i>=SP; --i) {
		s += RAM[i].repr() + " ";
	}
	return s;
}

// take word like '>>NAME' or '<<NAME' and jump to '@NAME'
void Interpreter::do_jump(const string &jumpword) {
	if(jumpword.substr(0,2) == ">>") {
		// forward jump, find word (>>NAME -> @NAME)
		while(true) {
			auto word = nextWordOrFail();
			if(word.substr(1) == jumpword.substr(2)) {
				return; // found word, stop
			}
		}
	}
	else if(jumpword.substr(0,2) == "<<") {
		// backward jump
		while(true) {
			auto word = prevWordOrFail();
			if(word.substr(1) == jumpword.substr(2)) {
				return; // found word, stop
			}
		}
	}
	else {
		throw LangError("Bad jumpword " + jumpword);
	}
}

string Interpreter::nextWordOrFail() {
	auto word = reader.nextWord();
	if(word == "") {
		throw LangError("Unexpected end of input");
	}
	return word;
}

string Interpreter::prevWordOrFail() {
	auto word = reader.prevWord();
	if(word == "") {
		throw LangError("Unable to find previous word");
	}
	return word;
}

void Interpreter::run() {
	// run one word at a time in a loop, with the reader position as the continuation
	while(true) {
		const string &word = reader.nextWord();
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
		smatch match;
		// integers just get pushed to the stack
		if(regex_match(word, match, *re_integer)) {
			push(newInt(stoi(word)));
			continue;
		}

		if(regex_match(word, match, *re_lambda)) {
			size_t index = stoi(match[1]);
			if (index < 0 || index >= LAMBDAS.size()) {
				throw LangError("Bad lambda index " + to_string(index));
			}
			push(newLambda(index));
			continue;
		}

		if(word == "return") {
			// return from word by popping back to previous wordlist (don't call at toplevel)
			if(reader.hasPushedWords()) {	
				reader.popWords();
			}
			continue;
		}

		if(word == "if") {
			// true jump is required
			auto true_jump = reader.nextWord();
			// false word is optional
			string false_jump;
			if(reader.peekWord().substr(0,2) == "<<" || reader.peekWord().substr(0,2) == ">>") {
				false_jump = reader.nextWord();
			}
			Object cond = pop();
			if(!cond.isBool()) {
				throw LangError("'if' requires true|false but got: " + cond.repr());
			}
			// these don't run the jump, they just reposition the reader
			if(cond.asBool()) {
				do_jump(true_jump);
			}
			else if(false_jump.size() > 0) {
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
			if(WORDS.find(name) != WORDS.end()) {
				throw LangError("Trying to redefine userword " + name);
			}
			// reserve count bytes
			int addr = MEM_NEXT;
			MEM_NEXT += count;
			// make name a word that returns the address so set! and ref can use the variable
			WORDS[name] = Wordlist{to_string(addr)};
			continue;
		}

		if(word == "del") {
			auto name = nextWordOrFail();
			auto userword = WORDS.find(name);
			if(userword == WORDS.end()) {
				throw LangError("Trying to delete non-existent userword " + name);
			}
			WORDS.erase(name);
			continue;
		}

		if(word == "call") {
			// top of stack must be a tagged wordlist ('lambda')
			auto val = pop();
			if(!val.isLambda()) {
				throw LangError("call expects a lambda, but got: " + val.repr());
			}
			// now this is just like calling a userword, below
			// TODO -- tail call elimination??
			reader.pushWords(LAMBDAS[val.asLambdaIndex()]);
			continue;
		}

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

		throw LangError(string("Unknown word ") + word);
	}
}
