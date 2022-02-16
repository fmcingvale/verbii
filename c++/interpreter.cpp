/*
	Interpreter - runs code.

	There is no compilation step, not really even a parsing step -- the interpreter
	runs directly from the objlists from the Reader. This makes the code smaller and
	makes e.g. forward declarations really easy since nothing is evaluated until it
	runs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include <iostream>
#include "interpreter.hpp"
#include "native.hpp"
#include "errors.hpp"
#include "xmalloc.hpp"
#include "string.h"
#include "langtypes.hpp"
using namespace std;

map<string,ObjList*> WORDS; // user defined words
vector<ObjList*> LAMBDAS;   // anonymous words
map<string,Object> VARS;

Interpreter::Interpreter() {

	syntax = new Syntax();

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
	syntax->addText(text);
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
		s += STACKLOCALS[i].fmtStackPrint() + " ";
	}
	return s;
}

// take symbol like '>>NAME' or '<<NAME' and jump to '@NAME'
void Interpreter::do_jump(const char *jumpword) {
	//cout << "DO_JUMP TO: " << jumpword << endl;
	if(!strncmp(jumpword, ">>", 2)) {
		// forward jump, find word (>>NAME -> @NAME)
		while(true) {
			auto obj = syntax->nextObjOrFail();
			//cout << "NEXT-WORD: " << word << endl;
			if(obj.isSymbol() && !strcmp(obj.asSymbol()+1, jumpword+2)) {
				//cout << "FOUND" << endl;
				return; // found word, stop
			}
		}
	}
	else if(!strncmp(jumpword, "<<", 2)) {
		// backward jump
		while(true) {
			auto obj = syntax->prevObjOrFail();
			//cout << "PREV-WORD: " << word << endl;
			if(obj.isSymbol() && !strcmp(obj.asSymbol()+1, jumpword+2)) {
				//cout << "FOUND" << endl;
				return; // found word, stop
			}
		}
	}
	else {
		throw LangError("Bad jumpword " + string(jumpword));
	}
}

void Interpreter::run(bool singlestep) {
	// run one word at a time in a loop, with the reader position as the continuation
	while(true) {
		auto obj = syntax->nextObj();
		// general note: there is kind of an artificial separation between words that are
		// recognized here, and words implemented in native.cpp. In priciple, all these words
		// could be implemented in native.cpp. However, I tend to put words here that directly
		// affect the interpreter inner state vs. functions that operate on data that go into
		// native.cpp. however, that's not always the case. basically, there's nothing magical
		// about why a word is here vs in native.cpp.
		// another consideration is how often words run -- better to have the most commonly run
		// words here since 'if(word == ...)' is a lot faster than a map lookup. But .. again ..
		// no hard and fast rule about it.
		if(obj.isNull()) {
			// i could be returning from a word that had no 'return',
			// so pop words like i would if it were a return
			if(syntax->hasPushedObjLists()) {
				syntax->popObjList();
				continue;
			}
			else {
				return;
			}
		}
		if(singlestep) {
			cout << "Run word: " << obj.fmtStackPrint() << endl;
			cout << "=> " << reprStack() << endl;
			string line;
			getline(cin, line);
		}
		
		// check for literal objects that just get pushed
		if(obj.isInt() || obj.isLambda() || obj.isString() || obj.isFloat()) {
			push(obj);
			continue;
		}

		if(obj.isSymbol("return")) {
			// return from word by popping back to previous wordlist (if not at toplevel)
			if(syntax->hasPushedObjLists()) {	
				syntax->popObjList();
			}
			else {
				return; // return from top level exits program
			}
			continue;
		}

		if(obj.isSymbol("if")) {
			// true jump is required
			auto true_jump = syntax->nextSymbolOrFail();
			//cout << "TRUE_JUMP: " << true_jump << endl;
			// false word is optional
			auto false_jump = syntax->peekObj();
			if(!false_jump.isSymbol("<<",2) && !false_jump.isSymbol(">>",2)) {
				false_jump = NULLOBJ;
			}
			//cout << "FALSE_JUMP: " << false_jump << endl;
			Object cond = pop();
			//cout << "POPPED COND: " << cond.repr() << endl;
			if(!cond.isBool()) {
				throw LangError("'if' requires true|false but got: " + cond.fmtStackPrint());
			}
			// these don't run the jump, they just reposition the reader
			if(cond.asBool()) {
				// no need to actually skip false jump since i'll be looking for '@'
				do_jump(true_jump.asSymbol());
			}
			else if(!false_jump.isNull()) {
				syntax->nextObj(); // only peeked it above
				do_jump(false_jump.asSymbol());
			}
			continue;
		}

		if(obj.isSymbol(">>",2) || obj.isSymbol("<<",2)) {
			do_jump(obj.asSymbol());
			continue;
		}

		if(obj.isSymbol("@",1)) {
			// jump target -- ignore
			continue;
		}

		if(obj.isSymbol("var")) {
			auto name = syntax->nextSymbolOrFail();
			auto count = syntax->nextObjOrFail();
			if(!count.isInt()) {
				throw LangError("Count must be int, got: " + count.fmtStackPrint());
			}
			// must be unique userword
			if(VARS.find(name.asSymbol()) != VARS.end()) {
				throw LangError("Trying to redefine variable " + name.fmtStackPrint());
			}
			// add to VARS so name lookup works (below) 
			VARS[name.asSymbol()] = newMemArray(count.asInt(), 0);
			continue;
		}

		if(obj.isSymbol("del")) {
			auto name = syntax->nextSymbolOrFail();
			auto userword = VARS.find(name.asSymbol());
			if(userword == VARS.end()) {
				throw LangError("Trying to delete non-existent variable " + name.fmtStackPrint());
			}
			VARS.erase(name.asSymbol());
			continue;
		}

		if(obj.isSymbol("call")) {
			// top of stack must be a lambda
			auto val = pop();
			if(!val.isLambda()) {
				throw LangError("call expects a lambda, but got: " + val.fmtStackPrint());
			}
			// now this is just like calling a userword, below
			// TODO -- tail call elimination??
			syntax->pushObjList(val.asLambda());
			continue;
		}

		// builtins, then userwords, then vars
		if(obj.isSymbol()) {
			auto bltin = BUILTINS.find(obj.asSymbol());
			if(bltin != BUILTINS.end()) {
				bltin->second(this);
				continue;
			}
			
			auto userword = WORDS.find(obj.asSymbol());
			if(userword != WORDS.end()) {
				// tail call elimination -- if i'm at the end of this wordlist OR next word is 'return', then
				// i don't need to come back here, so pop my wordlist first to stop stack from growing
				if(syntax->peekObj().isNull() || syntax->peekObj().isSymbol("return")) {
					if(syntax->hasPushedObjLists()) { // in case i'm at the toplevel
						syntax->popObjList();
					}
				}
				// execute word by pushing its wordlist and continuing
				syntax->pushObjList(userword->second);
				continue;
			}

			auto var = VARS.find(obj.asSymbol());
			if(var != VARS.end()) {
				push(var->second);
				continue;
			}
		}

		throw LangError(string("Unknown word ") + obj.fmtStackPrint());
	}
}
