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

Interpreter::Interpreter() {
	// stack at bottom, grows downward from SP_EMPTY
	// (the _EMPTY indexes are not valid storage locations, they indicate that the
	// respective stacks are empty)
	SP_MIN = 0;
	SP_EMPTY = SP_MIN + STACK_SIZE;
	SP = SP_EMPTY;

	// locals are next, grows downward from LP_EMPTY
	LP_MIN = SP_EMPTY;
	LP_EMPTY = LP_MIN + LOCALS_SIZE;
	LP = LP_EMPTY;

	// heap is next
	HEAP_START = LP_EMPTY;
	HEAP_END = HEAP_START + HEAP_STARTSIZE - 1;
	HEAP_NEXTFREE = HEAP_START;
	
	// allocate stack+locals+heap
	OBJMEM = (Object*)x_malloc((HEAP_END+1) * sizeof(Object));

	// init stats
	max_callstack = 0;
	min_run_SP = SP;
	min_run_LP = LP;
	nr_tailcalls = 0;
}

void Interpreter::print_stats() {	
	cout << "\n==== Runtime Stats ====" << endl;
	cout << "* General:\n";
	cout << "  Builtin words: " << BUILTINS.size() << endl;
	cout << "  User-defined words: " << WORDS.size() << endl;
	cout << "  Max stack depth: " << (SP_EMPTY - min_run_SP) << endl;
	cout << "  Max locals depth: " << (LP_EMPTY - min_run_LP) << endl;
	cout << "  Max callstack depth: " << max_callstack << endl;
	cout << "  Tail calls: " << nr_tailcalls << endl;

	cout << "* C++:\n";
#if defined(USE_GCMALLOC)
	GC_word pheap_size, pfree_bytes, punmapped_bytes, pbytes_since_gc, ptotal_bytes;
	GC_get_heap_usage_safe(&pheap_size, &pfree_bytes, &punmapped_bytes, &pbytes_since_gc, &ptotal_bytes);
	cout << "  Heap size: " << pheap_size << endl;
	cout << "  Free bytes: " << pfree_bytes << endl;
	cout << "  Unmapped bytes: " << punmapped_bytes << endl;
	cout << "  Bytes since gc: " << pbytes_since_gc << endl;
	cout << "  Total bytes: " << ptotal_bytes << endl;
#else
	cout << "  xmalloc bytes: " << X_BYTES_ALLOCATED << endl;
#endif
	cout << "  size of Object: " << sizeof(Object) << endl;

	cout << "* Notices:\n";
	if(SP != SP_EMPTY)
		cout << "  Stack is not empty! (" << (SP_EMPTY-SP) << " items)\n";
	if(LP != LP_EMPTY)
		cout << "  Locals are not empty! (" << (LP_EMPTY-LP) << " items)\n";
}

void Interpreter::push(Object obj) {
	if(SP <= SP_MIN) {
		throw LangError("Stack overflow");
	}
	OBJMEM[--SP] = obj;
	// stats
	min_run_SP = min(min_run_SP,SP);
}

Object Interpreter::pop() {
	if(SP >= SP_EMPTY) {
		throw LangError("Stack underflow");
	}
	return OBJMEM[SP++];
}

string Interpreter::reprStack() const {
	string s = "";
	for(int i=SP_EMPTY-1; i>=SP; --i) {
		s += OBJMEM[i].fmtStackPrint() + " ";
	}
	return s;
}

int Interpreter::heap_alloc(int nr) {
	if((HEAP_NEXTFREE + nr) >= HEAP_END) {
		// not enough memory, double it
		size_t newsize = max(HEAP_END+nr, (HEAP_END+1)*2);
		OBJMEM = (Object*)x_realloc(OBJMEM, newsize);
		HEAP_END = newsize - 1;
	}
	int addr = HEAP_NEXTFREE;
	Object zero = newInt(0);
	HEAP_NEXTFREE += nr;
	// init memory to zeros
	for(int i=0; i<nr; ++i) {
		OBJMEM[addr+i] = zero;
	}
	return addr;
}

Object Interpreter::nextCodeObj() {
	if(!code || codepos >= code->size()) {
		return newNull();
	}

	return code->at(codepos++);
}

Object Interpreter::nextCodeObjOrFail(const char *failmsg) {
	Object o = nextCodeObj();
	if(o.isNull()) {
		throw LangError("End of input: " + string(failmsg));
	}
	return o;
}

Object Interpreter::nextSymbolOrFail(const char *failmsg) {
	Object o = nextCodeObj();
	if(!o.isSymbol()) {
		throw LangError("Expecting symbol: " + string(failmsg));
	}
	return o;
}

Object Interpreter::peekNextCodeObj() {
	if(!code || codepos >= code->size()) {
		return newNull();
	}

	return code->at(codepos);
}

Object Interpreter::prevCodeObj() {
	if(!code || codepos == 0) {
		return newNull();
	}
	return code->at(--codepos);
}

Object Interpreter::prevCodeObjOrFail(const char *failmsg) {
	Object o = prevCodeObj();
	if(o.isNull()) {
		throw LangError("No previous object: " + string(failmsg));
	}
	return o;
}

// take symbol like '>>NAME' or '<<NAME' and jump to '@NAME'
void Interpreter::do_jump(const char *jumpword) {
	//cout << "DO_JUMP TO: " << jumpword << endl;
	if(!strncmp(jumpword, ">>", 2)) {
		// forward jump, find word (>>NAME -> @NAME)
		while(true) {
			auto obj = nextCodeObj();
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
			auto obj = prevCodeObj();
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

ObjList* Interpreter::lookup_word(const char *name) {
	auto userword = WORDS.find(name);
	if(userword != WORDS.end())
		return userword->second;
	else
		return NULL;
}

int Interpreter::lookup_var(const char *name) {
	auto uservar = VARS.find(name);
	if(uservar != VARS.end())
		return uservar->second;
	else
		return -1;
}

void Interpreter::code_call(ObjList *new_code) {
	if(!code) {
		throw LangError("code_call but no code is running");
	}
	callstack_code.push_back(code);
	callstack_pos.push_back(codepos);
	code = new_code;
	codepos = 0;
	// stats
	max_callstack = max(max_callstack,(int)callstack_code.size());
}

bool Interpreter::havePushedFrames() {
	return callstack_code.size() > 0;
}

void Interpreter::code_return() {
	if(!code) {
		throw LangError("code_return but no code is running");
	}

	// don't allow return from toplevel -- interpreter main loop needs to handle that case
	if(callstack_code.size() == 0) {
		throw LangError("code_return with empty callstack");
	}
	code = callstack_code.back();
	callstack_code.pop_back();
	codepos = callstack_pos.back();
	callstack_pos.pop_back();
}

bool Interpreter::hasWord(const char *name) {
	return (VARS.find(name) != VARS.end() || WORDS.find(name) != WORDS.end());
}

void Interpreter::deleteWord(const char* name) {
	auto var = VARS.find(name);
	if(var != VARS.end()) {
		VARS.erase(name);
		return;
	}
	auto userword = WORDS.find(name);
	if(userword != WORDS.end()) {
		WORDS.erase(name);
		return;
	}

	throw LangError("Trying to delete non-existent name: " + string(name));
}

void Interpreter::run(ObjList *to_run, void (*debug_hook)(Interpreter*, Object)) {
	if(code) {
		throw LangError("Interpreter run() called recursively");
	}

	code = to_run;
	codepos = 0;

	// run one word at a time in a loop, with the reader position as the continuation
	while(true) {
		Object obj = nextCodeObj();
		if(debug_hook)
			debug_hook(this, obj);

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
			//if(syntax->hasPushedObjLists()) {
			//	syntax->popObjList();
			//	continue;
			//}
			if(havePushedFrames()) {
				code_return();
				continue;
			}
			else {
				code = NULL; // mark self as not running
				return;
			}
		}
		
		// check for literal objects that just get pushed
		if(obj.isInt() || obj.isLambda() || obj.isString() || obj.isFloat()) {
			push(obj);
			continue;
		}

		if(obj.isSymbol("'",1)) {
			// quoted symbol - remove one level of quoting and push
			push(newSymbol(obj.asSymbol()+1, strlen(obj.asSymbol())-1));
			continue;
		}

		if(obj.isSymbol("return")) {
			// return from word by popping back to previous wordlist (if not at toplevel)
			//if(syntax->hasPushedObjLists()) {	
			//	syntax->popObjList();
			//}
			if(havePushedFrames()) {
				code_return();
			}
			else {
				code = NULL;
				return; // return from top level exits program
			}
			continue;
		}

		if(obj.isSymbol("if")) {
			// true jump is next
			auto true_jump = nextSymbolOrFail("expecting jump after 'if'");
			//cout << "TRUE_JUMP: " << true_jump << endl;
			Object cond = pop();
			//cout << "POPPED COND: " << cond.repr() << endl;
			if(!cond.isBool()) {
				throw LangError("'if' requires true|false but got: " + cond.fmtStackPrint());
			}
			// this doesn't run the jump, it just repositions the stream
			if(cond.asBool()) {
				// no need to actually skip false jump since i'll be looking for '@'
				do_jump(true_jump.asSymbol());
			}
			// if false, just continue with next instruction
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
			auto name = nextSymbolOrFail("expecting symbol after 'var'");
			auto count = nextCodeObjOrFail("expecting count after 'var'");
			if(!count.isInt()) {
				throw LangError("Count must be int, got: " + count.fmtStackPrint());
			}
			// must be unique userword
			if(hasWord(name.asSymbol())) {
				throw LangError("Trying to redefine name: " + name.fmtStackPrint());
			}
			// add to VARS so name lookup works (below) 
			VARS[name.asSymbol()] = heap_alloc(count.asInt());
			continue;
		}

		if(obj.isSymbol("del")) {
			auto name = nextSymbolOrFail("expecting symbol after 'del'");
			deleteWord(name.asSymbol());
			continue;
		}

		if(obj.isSymbol("call")) {
			// top of stack must be a lambda OR a list
			auto val = pop();
			if(val.isLambda()) {
				// now this is just like calling a userword, below
				// TODO -- tail call elimination??
				//syntax->pushObjList(val.asLambda());
				code_call(val.asLambda());
			}
			else if(val.isList()) {
				// same as above
				code_call(val.asList());
			}
			else {
				throw LangError("call expects a lambda, but got: " + val.fmtStackPrint());
			}
			continue;
		}

		// builtins, then userwords, then vars
		if(obj.isSymbol()) {
			auto bltin = BUILTINS.find(obj.asSymbol());
			if(bltin != BUILTINS.end()) {
				bltin->second(this);
				continue;
			}
			
			ObjList *wordlist = lookup_word(obj.asSymbol());
			if(wordlist) {
				// tail call elimination -- if i'm at the end of this wordlist OR next word is 'return', then
				// i don't need to come back here, so pop my wordlist first to stop stack from growing
				#if 1 // can turn off to test without tail call elimination, if desired
				if(peekNextCodeObj().isNull() || peekNextCodeObj().isSymbol("return")) {
					if(havePushedFrames()) { // in case i'm at the toplevel
						code_return();
						++nr_tailcalls;
					}
				}
				#endif
				// execute word by pushing its objlist and continuing
				code_call(wordlist);
				//syntax->pushObjList(wordlist);
				continue;
			}

			int addr = lookup_var(obj.asSymbol());
			if(addr >= 0) {
				push(newInt(addr));
				continue;
			}
		}

		throw LangError(string("Unknown word ") + obj.fmtStackPrint());
	}
}
