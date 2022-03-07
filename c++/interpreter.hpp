/*
	Interpreter - runs code.

	There is no compilation step, not really even a parsing step -- the interpreter
	runs directly from the wordlists from the Reader. This makes the code smaller and
	makes e.g. forward declarations really easy since nothing is evaluated until it
	runs.

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

const int STACK_SIZE = (1<<10);
const int LOCALS_SIZE = (1<<10);

class Interpreter {
	public:
	Interpreter();

	//void addText(const std::string &text);

	// see notes in langtypes.hpp -- Object is meant to be passed by value
	void push(Object obj);
	Object pop();

	// get representation of stack for printing
	std::string reprStack() const;

	void run(ObjList *to_run, bool singlestep=false);

	// all are public so builtins can use without a hassle
	std::map<std::string,ObjList*> WORDS; // user-defined words
	std::map<std::string,Object> VARS; // user-defined variables

	// 3 memory areas: stack, locals, free memory
	//
	// the stack and locals are of fixed size, so are allocated in one
	// block. normal integer values are used to address them.
	Object *STACKLOCALS;
	int SIZE_STACKLOCALS;
	
	// within STACKLOCAKS, the stack & locals are defined by these indices:

	// current stack pointer (points to item on top of stack), empty value and lowest usable index
	int SP, SP_EMPTY, SP_MIN;
	// same for locals
	int LP, LP_EMPTY, LP_MIN;

	// program-allocated memory is stored as a MemoryArray instead of being
	// part of a large block allocated here. having separately allocated
	// objects for each memory array is bad for cache but very good in the
	// sense that the GC takes care of it and i don't have to track which entries
	// in a large array are still valid, etc. use newMemoryArray() or copyMemoryArray()
	// to get program-allocated memory

	// lookup user-defined word or NULL if not found
	ObjList* lookup_word(const char *name);
	// get memory array or return null object
	Object lookup_var(const char *name);

	void do_jump(const char *jumpword);

	//Syntax *syntax;

	Object nextCodeObj();
	Object nextCodeObjOrFail(const char *failmsg);
	Object peekNextCodeObj();
	Object prevCodeObj();
	Object prevCodeObjOrFail(const char *failmsg);

	Object nextSymbolOrFail(const char *failmsg);

	void code_call(ObjList *new_code);
	void code_return();
	
	// current running code & callstack of previous frames
	ObjList *code; // NULL if not code loaded
	size_t codepos;
	std::vector<ObjList*> callstack_code;
	std::vector<size_t> callstack_pos;
};


