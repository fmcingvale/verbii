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

const int RAM_SIZE = (1<<24);
const int STACK_SIZE = (1<<16);
const int LOCALS_SIZE = (1<<10);

extern std::map<std::string,Wordlist> WORDS;
extern std::vector<Wordlist*> LAMBDAS;

class Interpreter : public gc {
	public:
	Interpreter();

	void addText(const std::string &text);

	// see notes in langtypes.hpp -- Object is meant to be passed by value
	void push(Object obj);
	Object pop();

	// get representation of stack for printing
	std::string reprStack() const;

	void run();

	// all are public so builtins can use without a hassle
	Reader reader;
	// 3 memory areas: stack, locals, free memory
	//
	// they must be contiguous (because i.e. 'ref' and 'set!' don't know what
	// type of address they are using). they can be in any layout though.
	//
	std::array<Object,RAM_SIZE> RAM;
	// RAM indexes: stack pointer, empty value and lowest usable index
	int SP, SP_EMPTY, SP_MIN;
	// same for locals 
	int LP, LP_EMPTY, LP_MIN;
	// next memory address available and last usable
	int MEM_NEXT, MEM_LAST;
	// regexes 
	std::regex *re_integer;
	std::regex *re_lambda;

	void do_jump(const std::string &jumpword);

	// to avoid a lot of 'if word==""' checks, these require a non-empty
	// word or they throw an exception ... for use in cases where there MUST
	// be a next/previous word, or its a syntax error
	std::string nextWordOrFail();
	std::string prevWordOrFail();
};


