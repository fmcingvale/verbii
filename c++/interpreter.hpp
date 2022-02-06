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
#include "reader.hpp"
#include "langtypes.hpp"

const int STACK_SIZE = (1<<10);
const int LOCALS_SIZE = (1<<10);

extern std::map<std::string,Wordlist> WORDS;
extern std::vector<Wordlist*> LAMBDAS;

class Interpreter {
	public:
	Interpreter();

	void addText(const std::string &text);

	// see notes in langtypes.hpp -- Object is meant to be passed by value
	void push(Object obj);
	Object pop();

	// get representation of stack for printing
	std::string reprStack() const;

	void run(bool singlestep=false);

	// all are public so builtins can use without a hassle
	Reader reader;
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

	void do_jump(const std::string &jumpword);

	// to avoid a lot of 'if word==""' checks, these require a non-empty
	// word or they throw an exception ... for use in cases where there MUST
	// be a next/previous word, or its a syntax error
	const std::string &nextWordOrFail();
	const std::string &prevWordOrFail();
};


