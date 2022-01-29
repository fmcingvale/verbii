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
}

void Interpreter::addText(const string &text) {
	reader.addText(text);
}

void Interpreter::push(tagged obj) {
	if(SP <= SP_MIN) {
		throw LangError("Stack overflow");
	}
	RAM[--SP] = obj;
}

tagged Interpreter::pop() {
	if(SP >= SP_EMPTY) {
		throw LangError("Stack underflow");
	}
	return RAM[SP++];
}

string Interpreter::reprStack() const {
	string s = "";
	for(int i=SP_EMPTY-1; i>=SP; --i) {
		s += reprTagged(RAM[i]) + " ";
	}
	return s;
}

void Interpreter::do_jump(const string &jumpword) {
	if(jumpword.substr(0,2) == ">>") {
		// forward jump, find word (>>NAME -> @NAME)
		while(true) {
			auto word = reader.nextWord();
			if(word == "") {
				throw new LangError("Can't find jump target for " + jumpword);
			}
			else if(word.substr(1) == jumpword.substr(2)) {
				return; // found word, stop
			}
		}
	}
	else if(jumpword.substr(0,2) == "<<") {
		// backward jump
		while(true) {
			auto word = reader.prevWord();
			if(word == "") {
				throw new LangError("Can't find jump target for " + jumpword);
			}
			else if(word.substr(1) == jumpword.substr(2)) {
				return; // found word, stop
			}
		}
	}
	else {
		throw LangError("Bad jumpword " + jumpword);
	}
}

void Interpreter::run() {
	// run one word at a time in a loop, with the reader position as the continuation
	while(true) {
		const string &word = reader.nextWord();
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
		if(regex_match(word, match, *re_integer)) {
			push(intToTagged(stoi(word)));
			continue;
		}

		if(word == "return") {
			// return from word by popping back to previous wordlist
			reader.popWords();
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
			bool cond = taggedToBool(pop());
			// these don't run the jump, they just reposition the reader
			if(cond) {
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
