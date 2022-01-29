#include <iostream>
#include "interpreter.hpp"
#include "native.hpp"
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

void Interpreter::push(uint32 obj) {
	if(SP <= SP_MIN) {
		printf("*** STACK OVERFLOW ***\n");
		return;
	}
	RAM[--SP] = obj;
}

uint32 Interpreter::pop() {
	if(SP >= SP_EMPTY) {
		printf("*** STACK UNDERFLOW ***\n");
		return -1;
	}
	return RAM[SP++];
}

std::string Interpreter::nextWord() {
	return reader.nextWord();
}	

string Interpreter::repr_stack() const {
	string s = "";
	for(int i=SP_EMPTY-1; i>=SP; --i) {
		s += to_string(taggedToInt(RAM[i])) + " ";
	}
	return s;
}

void Interpreter::run() {
	while(1) {
		const string &word = reader.nextWord();
		if(word == "") {
			// i could be returning from a word that had no 'return',
			// to pop words like i would if it were return
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
				reader.popWords();
			}
			// execute word by pushing its wordlist and continuing
			reader.pushWords(&userword->second);
			continue;
		}

		cout << "*** UNKNOWN WORD: " << word << endl;
		return;
	}
}
