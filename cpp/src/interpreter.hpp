/*
	Interpreter - runs code.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once
#include <array>
#include <cstdio>
#include <string>
#include <regex>
#include <map>
#include "langtypes.hpp"
//#include "reader.hpp"
//#include "syntax.hpp"

// setting these small for now to help detect runaway code filling up the stack.
const int STACK_SIZE = (1<<16);
const int HEAP_STARTSIZE = (1<<16);

const int MAX_CALLSTACK_DEPTH = 1024;

// define to turn OFF support for >> and << (must be compiled into opcodes)
#define JUMP_OPCODES_ONLY 1

struct CallStackEntry {
	ObjList *code;
	int pos;
	CallFrameData *framedata;
};

class Interpreter {
	public:
	Interpreter();

	//void addText(const std::string &text);

	// see notes in langtypes.hpp -- Object is meant to be passed by value
	void push(Object obj);
	Object pop();

	// get representation of stack for printing
	std::string reprStack() const;

	void run(ObjList *to_run, void (*debug_hook)(Interpreter*, Object)=NULL);

	// TODO -- get rid of split between VARS and WORDS ... all should be WORDS
	
	// all are public so builtins can use without a hassle
	
	// 2 memory areas: stack & free memory
	//
	// the stack is of fixed size, so it lives at the bottom of memory.
	// program-allocated memory (vars) lives above it in a space that will be
	// reallocated as needed
	//
	// plain integers are used to address all 3 areas
	Object *OBJMEM;
	
	// current stack pointer (points to item on top of stack), empty value and lowest usable index
	int SP, SP_EMPTY, SP_MIN;
	// starting index for program-allocatable memory
	int HEAP_START;
	// last valid index
	int HEAP_END;
	// next available index to allocate
	int HEAP_NEXTFREE;

	// allocate heap memory to store nr Objects - returns starting index
	int heap_alloc(int nr);

	// lookup user-defined word or NULL if not found
	ObjList* lookup_word(const char *name);

	#ifndef JUMP_OPCODES_ONLY
	void do_jump(const char *jumpword);
	#endif

	bool hasBuiltin(const char *name);

	// user-defined words
	bool hasWord(const char *name);	
	void defineWord(const char *name, ObjList *objlist, bool allow_overwrite);
	void deleteWord(const char* name);
	Object getWordlist(); // returns a List

	//Syntax *syntax;

	Object nextCodeObj();
	Object nextCodeObjOrFail(const char *failmsg);
	Object peekNextCodeObj();
	Object prevCodeObj();
	Object prevCodeObjOrFail(const char *failmsg);

	Object nextSymbolOrFail(const char *failmsg);

	// if the code is from a BoundLambda, pass it as the 3rd parameter
	void code_call(ObjList *new_code, BoundLambda *bound_lambda=NULL);
	bool havePushedFrames();
	void code_return();

	// stack of current and previous frames
	CallStackEntry callstack[MAX_CALLSTACK_DEPTH];
	int callstack_cur; // index of currently running frame or -1

	// shortcuts for current frame
	ObjList *code;
	size_t codepos;
	CallFrameData *framedata;

	// stats
	bool PROFILE_CALLS;
	void print_stats();
	int max_callstack;
	int min_run_SP;
	unsigned long nr_tailcalls;
	int max_frame_slot_used;
	int nr_total_calls;
	int nr_saved_frames;

	protected:
	// use functions above so error handling can be in one place
	std::map<std::string,ObjList*> WORDS; // user-defined words

	// when profiling, this is the number of times a word (either builtin or user-defined) is called
	std::map<std::string,int> WORD_CALLS;
	void print_word_calls();
};


