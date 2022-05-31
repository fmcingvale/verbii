//
// An experimental minimized front end, seeing how much of the repl
// I can push into verbii code
//
// Copyright (c) 2022 Frank McIngvale, see LICENSE
//

#include "interpreter.hpp"
#include "errors.hpp"
#include <iostream>
#include <fstream>
#include "xmalloc.hpp"
#include "deserialize.hpp"
#include "native.hpp"
using namespace std;

static const char *BOOTFILE = "../lib/boot.verb.b";

void backtrace_curframe(Interpreter *intr) {
	string trace = "";
	int nr = 7; // number of words to print in each frame
	while(nr--) {
		auto o = intr->prevCodeObj();
		if(o.isVoid()) {
			cout << trace << endl;
			return;
		}
		else {
			trace = o.fmtStackPrint() + ' ' + trace;
		}
	}
	cout << trace << endl;
}

void print_backtrace(Interpreter *intr) {
	int i=0;
	while(1) {
		cout << "FRAME " << i++ << ": ";
		backtrace_curframe(intr);
		if(intr->havePushedFrames()) {
			intr->code_return();
		}
		else {
			return;
		}
	}
}

void deserialize_and_run(Interpreter *intr, string filename) {
	ifstream fileIn(filename);
	deserialize_stream(intr, fileIn);
	// run __main__ to setup any vars
	auto code = intr->lookup_word("__main__");
	if(!code) 
		throw LangError("Unable to find __main__ after deserializing: " + filename);

	// delete __main__ *BEFORE* running it, since the code I'm about to run may
	// want to define __main__ itself
	intr->deleteWord("__main__");

	intr->run(code);
}

int main(int argc, char *argv[]) {
	x_mem_init();

	bool SHOW_RUN_STATS = false;
	native_cmdline_args = newList();

	for(int i=1; i<argc; ++i) {
		if(!strcmp(argv[i], "-stats")) {
			SHOW_RUN_STATS = true;
		}
		else {
			native_cmdline_args.data.objlist->push_back(newString(argv[i]));
		}
	}
	
	Interpreter *intr = NULL;
	while(1) {
		try {
			intr = new Interpreter();
			deserialize_and_run(intr, BOOTFILE);
			break;
		}
		catch (LangError &err) {
			auto errstr = "*** " + string(err.what()) + " ***";
			print_backtrace(intr);
			printf("%s\n", errstr.c_str());
			if(EXIT_ON_EXCEPTION)
				exit(1);
		}
	}
}

