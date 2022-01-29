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
#include "tagging.hpp"

const int RAM_SIZE = (1<<24);
const int STACK_SIZE = (1<<16);
const int LOCALS_SIZE = (1<<10);

extern std::map<std::string,Wordlist> WORDS;

class Interpreter {
	public:
	Interpreter();

	void addText(const std::string &text);

	void push(uint32 obj);
	uint32 pop();

	// provided for extensions that need to access the reader
	std::string nextWord();

	std::string repr_stack() const;

	void run();

	protected:
	Reader reader;
	std::array<uint32,RAM_SIZE> RAM;
	// 3 memory areas: stack, locals, free memory
	// RAM indexes: stack pointer, empty value and lowest usable index
	int SP, SP_EMPTY, SP_MIN;
	// same for locals 
	int LP, LP_EMPTY, LP_MIN;
	// next memory address available and last usable
	int MEM_NEXT, MEM_LAST; 
	std::regex *re_integer;
};


