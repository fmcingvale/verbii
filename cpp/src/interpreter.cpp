/*
	Interpreter - runs code.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include <iostream>
#include "interpreter.hpp"
#include "native.hpp"
#include "errors.hpp"
#include "xmalloc.hpp"
#include "string.h"
#include "langtypes.hpp"
#include "opcodes.hpp"
#include <algorithm>
using namespace std;

// put this here so i don't have to include opcode.hpp in interpreter.hpp
static int OPCODE_CALLCOUNTS[OPCODE_LAST_PLUS1];

Interpreter::Interpreter() {
	// stack at bottom, grows downward from SP_EMPTY
	// (the _EMPTY indexes are not valid storage locations, they indicate that the
	// respective stacks are empty)
	SP_MIN = 0;
	SP_EMPTY = SP_MIN + STACK_SIZE;
	SP = SP_EMPTY;

	// heap is next
	HEAP_START = SP_EMPTY;
	HEAP_END = HEAP_START + HEAP_STARTSIZE - 1;
	HEAP_NEXTFREE = HEAP_START;
	
	// allocate stack+heap
	OBJMEM = (Object*)x_malloc((HEAP_END+1) * sizeof(Object));

	// init callstack
	for(int i=0; i<MAX_CALLSTACK_DEPTH; ++i)
		callstack[i].framedata = new CallFrameData();

	callstack_cur = -1;

	// init stats
	max_callstack = 0;
	min_run_SP = SP;
	nr_tailcalls = 0;
	max_frame_slot_used = 0;
	nr_total_calls = 0;
	PROFILE_CALLS = false;
	for(int i=0; i<OPCODE_LAST_PLUS1; ++i)
		OPCODE_CALLCOUNTS[i] = 0;
}

void Interpreter::print_word_calls() {
	size_t MAX_WORDS = 20;
	cout << "  Word calls (top " << MAX_WORDS << "):\n";

	// sort by call frequency
	vector<tuple<string,int>> clist;
	for(const auto& ent: WORD_CALLS)
		clist.push_back(make_tuple(ent.first,ent.second));

	sort(clist.begin(), clist.end(), [](tuple<string,int> a,tuple<string,int> b) { return get<1>(a) > get<1>(b); });

	// only show first MAX_WORDS
	if(clist.size() > MAX_WORDS)
		clist.resize(MAX_WORDS);

	for(const auto& tup: clist) {
		cout << "    " << get<0>(tup) << ": " << get<1>(tup) << endl;
	}
}

#include "util.hpp"

void Interpreter::print_stats() {	
	cout << "\n==== Runtime Stats ====" << endl;
	cout << "* General:\n";
	cout << "  Builtin words: " << BUILTINS.size() << endl;
	cout << "  User-defined words: " << WORDS.size() << endl;
	cout << "  Max stack depth: " << (SP_EMPTY - min_run_SP) << endl;
	cout << "  Max callstack depth: " << max_callstack << endl;
	cout << "  Max callframe data slot: " << max_frame_slot_used << endl;
	cout << "  Saved frames: " << nr_saved_frames << endl;
	cout << "  Total calls: " << nr_total_calls << endl;
	cout << "  Tail calls: " << nr_tailcalls << endl;
	
	double tottime = current_system_cpu_time() - STARTUP_TIME;
	cout << "  Total time: " << tottime << endl;

	if(PROFILE_CALLS) {
		cout << "Opcode calls:" << endl;
		for(int i=0; i<OPCODE_LAST_PLUS1; ++i) 
			cout << opcode_code_to_name(i) << ": " << OPCODE_CALLCOUNTS[i] << endl;

		cout << "Top word calls:" << endl;
		print_word_calls();
	}

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
	if(SP != SP_EMPTY) {
		cout << "  Stack is not empty! (" << (SP_EMPTY-SP) << " items)\n";
		cout << " => " << reprStack() << endl;
	}
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
		OBJMEM = (Object*)x_realloc(OBJMEM, newsize*sizeof(Object));
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
		return newVoid();
	}

	return code->at(codepos++);
}

Object Interpreter::nextCodeObjOrFail(const char *failmsg) {
	Object o = nextCodeObj();
	if(o.isVoid()) {
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
		return newVoid();
	}

	return code->at(codepos);
}

Object Interpreter::prevCodeObj() {
	if(!code || codepos == 0) {
		return newVoid();
	}
	return code->at(--codepos);
}

Object Interpreter::prevCodeObjOrFail(const char *failmsg) {
	Object o = prevCodeObj();
	if(o.isVoid()) {
		throw LangError("No previous object: " + string(failmsg));
	}
	return o;
}

#ifndef JUMP_OPCODES_ONLY
// take symbol like '>>NAME' or '<<NAME' and jump to '@NAME'
void Interpreter::do_jump(const char *jumpword) {
	//cout << "DO_JUMP TO: " << jumpword << endl;
	if(!strncmp(jumpword, ">>", 2)) {
		// forward jump, find word (>>NAME -> @NAME)
		while(true) {
			auto obj = nextCodeObj();
			if(obj.isVoid())
				throw LangError("No such jump: " + string(jumpword));
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
			if(obj.isVoid())
				throw LangError("No such jump: " + string(jumpword));
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
#endif // JUMP_OPCODES_ONLY

ObjList* Interpreter::lookup_word(const char *name) {
	auto userword = WORDS.find(name);
	if(userword != WORDS.end())
		return userword->second;
	else
		return NULL;
}

void Interpreter::code_call(ObjList *new_code, BoundLambda *bound_lambda) {
	if(callstack_cur < 0)
		throw LangError("code_call but no code is running");
	else if(callstack_cur >= (MAX_CALLSTACK_DEPTH-1))
		throw LangError("Max callstack depth exceeded");

	++nr_total_calls;

	// save current context -- only .pos needs to be updated
	callstack[callstack_cur].pos = codepos;

	// setup new frame
	++callstack_cur;
	callstack[callstack_cur].code = new_code;
	callstack[callstack_cur].pos = 0;
	// clear framedata to help GC
	callstack[callstack_cur].framedata->clear_data();
	
	// set shortcuts to new frame
	code = new_code;
	codepos = 0;
	framedata = callstack[callstack_cur].framedata;

	// when the bound lambda was created, the current frame (at the time) was saved
	// as its .outer frame. when the bound lambda runs here in a new frame, it needs
	// to have its .outer frame connected to the same .outer as when it was created,
	// so it has access to the saved data (closure)
	if(bound_lambda)
		framedata->setOuterFrame(bound_lambda->outer);

	// stats
	max_callstack = max(max_callstack,(int)(callstack_cur+1));
}

bool Interpreter::havePushedFrames() {
	return callstack_cur >= 0;
}

void Interpreter::code_return() {
	if(!callstack_cur < 0)
		throw LangError("code_return but no code is running");

	// did framedata become bound?
	if(callstack[callstack_cur].framedata->isBound()) {
		// framedata has been bound to one or more lambdas, so it cannot be 
		// freed. replace it in the stack with a new frame.
		callstack[callstack_cur].framedata = new CallFrameData();
		// stats
		++nr_saved_frames;
	}

	// pop frame
	--callstack_cur;
	// set shortcuts, IF that wasn't the last frame (if it was, then these won't be used again)
	if(callstack_cur >= 0) {
		code = callstack[callstack_cur].code;
		codepos = callstack[callstack_cur].pos;
		framedata = callstack[callstack_cur].framedata;
	}
}

bool Interpreter::hasWord(const char *name) {
	return WORDS.find(name) != WORDS.end();
}

bool Interpreter::hasBuiltin(const char *name) {
	return BUILTINS.find(name) != BUILTINS.end();
}

void Interpreter::defineWord(const char *name, ObjList *objlist, bool allow_overwrite) {
	// don't allow defining a name with the same name as a builtin -- the builtin would
	// still run so user wouldn't know why their code wasn't having an effect
	if((hasBuiltin(name) || hasWord(name)) && !allow_overwrite)
		throw LangError("Trying to redefine name: " + string(name));

	WORDS[name] = objlist;
}

void Interpreter::deleteWord(const char* name) {
	// **NOTE** strings containing NULLs won't work as keys here
	auto userword = WORDS.find(name);
	if(userword != WORDS.end()) {
		WORDS.erase(name);
		return;
	}

	throw LangError("Trying to delete non-existent name: " + string(name));
}

Object Interpreter::getWordlist() {
	Object list = newList();
	for(const auto& pair : WORDS) {
		list.data.objlist->push_back(newSymbol(pair.first));
	}
	return list;
}

void Interpreter::run(ObjList *to_run, void (*debug_hook)(Interpreter*, Object)) {
	if(callstack_cur >= 0)
		throw LangError("Interpreter run() called recursively");

	if(!to_run)
		throw LangError("Got NULL* as code in run()");

	// almost but not quite code_call() so have to do it here
	callstack_cur = 0;
	callstack[callstack_cur].code = to_run;
	callstack[callstack_cur].pos = 0;
	// clear data to help GC
	callstack[callstack_cur].framedata->clear_data();

	// set shortcuts that are used everywhere else
	code = to_run;
	codepos = 0;
	framedata = callstack[callstack_cur].framedata;

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

		// TODO -- make this all a case on .type ...

		if(obj.isOpcode()) {
			uint8_t code, A;
			uint16_t B;
			uint32_t C;
			opcode_unpack(obj.asOpcode(), code, A, B, C);
			if(code < 0 || code >= OPCODE_LAST_PLUS1)
				throw LangError("Bad opcode: " + to_string(code));

			OPCODE_FUNCTIONS[code](this, A, B, C);
			++OPCODE_CALLCOUNTS[code];
			continue;
		}
		else if(obj.isSymbol()) {
			if(obj.isSymbol("'",1)) {
				// quoted symbol - remove one level of quoting and push
				push(newSymbol(obj.asSymbol()->as_cstr()+1, obj.asSymbol()->length()-1));
				continue;
			}
			
			if(obj.isSymbol("return")) {
				// return from word by popping back to previous wordlist
				code_return();
				// if exited top level, exit program
				if(callstack_cur < 0)
					return;
				
				continue;
			}

			if(obj.isSymbol("if")) {
				Object cond = pop();
				//cout << "POPPED COND: " << cond.repr() << endl;
				if(!cond.isBool()) {
					throw LangError("'if' requires true|false but got: " + cond.fmtStackPrint());
				}
				// if TRUE, continue with the next instruction (so do nothing here)
				// if FALSE, skip the next instruction
				if(!cond.asBool()) {
					codepos += 1;
				}
				continue;
			}

			#ifndef JUMP_OPCODES_ONLY
			if(obj.isSymbol(">>",2) || obj.isSymbol("<<",2)) {
				do_jump(obj.asSymbol());
				continue;
			}
			#endif

			if(obj.isSymbol("@",1)) {
				// jump target -- ignore
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
				else if(val.isBoundLambda()) {
					// as above but pass bound lambda so its new call frame will be
					// connected the same outer frame that was captured with bind-lambda
					code_call(val.data.boundLambda->objlist, val.data.boundLambda);
				}
				else {
					throw LangError("call expects a lambda or bound-lambda, but got: " + val.fmtStackPrint());
				}
				continue;
			}

			// builtins, then userwords
			if(obj.isSymbol()) {
				// **NOTE** strings containing NULLs won't work here
				auto bltin = BUILTINS.find(obj.asSymbol()->as_cstr());
				if(bltin != BUILTINS.end()) {
					bltin->second(this);
					if(PROFILE_CALLS) {
						auto w = WORD_CALLS.find(obj.asSymbol()->as_cstr());
						if(w == WORD_CALLS.end())
							WORD_CALLS[obj.asSymbol()->as_cstr()] = 1; // new entry
						else
							WORD_CALLS[obj.asSymbol()->as_cstr()] += 1;
					}
					continue;
				}
				
				// **NOTE** strings containing NULLs won't work here
				ObjList *wordlist = lookup_word(obj.asSymbol()->as_cstr());
				if(wordlist) {
					// tail call elimination -- if i'm at the end of this wordlist OR next word is 'return', then
					// i don't need to come back here, so pop my wordlist first to stop stack from growing
					#if 1 // can turn off to test without tail call elimination, if desired
					// FIXME - don't eliminate at toplevel since code_call expects a non-empty stack
					if((callstack_cur >= 1) && (peekNextCodeObj().isVoid() || peekNextCodeObj().isSymbol("return"))) {
						code_return();
						++nr_tailcalls;
					}
					#endif
					// execute word by pushing its objlist and continuing
					code_call(wordlist);
					//syntax->pushObjList(wordlist);
					if(PROFILE_CALLS) {
						auto w = WORD_CALLS.find(obj.asSymbol()->as_cstr());
						if(w == WORD_CALLS.end())
							WORD_CALLS[obj.asSymbol()->as_cstr()] = 1; // new entry
						else
							WORD_CALLS[obj.asSymbol()->as_cstr()] += 1;
					}
					continue;
				}
			}

			throw LangError("Unknown word " + obj.fmtDisplay());
		}
		
		else if(obj.isVoid()) {
			// i could be returning from a word that had no 'return',
			// so pop words like i would if it were a return

			// note a subtle (unintended) side effect here:
			//		10 20 void 30 40 5 make-list call
			// execution will stop after 20 since void makes the interpreter think it has reached
			// the end of the list. avoiding this by making 'return' mandatory runs into trouble for
			// dynamically created lists that are called -- doesn't seem worth it to have to check every
			// list before calling that it ends with 'return' and modifying it if not.
			// bottom line -- storing void (in ANY container) is a bad idea and this is just an d
			// example of one consequence

			code_return();
			if(callstack_cur < 0)
				return; // popped top frame, return
			
			continue;
		}
		
		// if object was created from a list literal ( [ ... ] ), then it must be deepcopied
		// (see DESIGN-NOTES.md).
		else if(obj.isList()) {
			push(obj.deepcopy());
			continue;
		}

		// everything else gets pushed -- initially i was only pushing objects that could be
		// parsed from source. however, given the dynamic nature of verbii, every type of
		// of object can end up in runnable code ... for example, dictionaries are not 
		// parseable from source but:
		//	[ ] make-dict 1 make-list make-lambda 
		// ... now the dictionary object is in runnable code
		else {
			push(obj);
			continue;
		}
		
		//throw LangError(string("Unknown word ") + obj.fmtDisplay());
	}
}
