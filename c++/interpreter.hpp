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

const int STACK_SIZE = (1<<10);
const int LOCALS_SIZE = (1<<10);
const int HEAP_STARTSIZE = (1<<16);

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

	// all are public so builtins can use without a hassle
	std::map<std::string,ObjList*> WORDS; // user-defined words
	std::map<std::string,int> VARS; // user-defined variables, name -> index into OBJMEM

	// 3 memory areas: stack, locals, free memory
	//
	// the stack & locals are both of fixed size, so live at the bottom of memory.
	// program-allocated memory (vars) lives above them in a space that will be
	// reallocated as needed
	//
	// plain integers are used to address all 3 areas
	Object *OBJMEM;
	
	// current stack pointer (points to item on top of stack), empty value and lowest usable index
	int SP, SP_EMPTY, SP_MIN;
	// same for locals
	int LP, LP_EMPTY, LP_MIN;
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
	// get memory address (index into OBJMEM) or return -1
	int lookup_var(const char *name);

	void do_jump(const char *jumpword);

	void deleteWord(const char* name);

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


