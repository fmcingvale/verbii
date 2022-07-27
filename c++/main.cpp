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
	if(!fileIn)
		throw LangError("No such file: " + filename);

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

#include <limits.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
	// under gcc 9.4 this is not necessary -- STARTUP_TIME is set automatically from
	// its declaration. however it fails under clang 7.5 so init it explicitly ...
	STARTUP_TIME = chrono::steady_clock::now();
		
	x_mem_init();

	bool SHOW_RUN_STATS = false;
	bool DO_PROFILING = false;
	// collect args that should be passed on to boot.verb, filtering out mine
	auto cmdline_args = newList();

	// catch only the flags that have to be implemented natively
	// pass the rest through as-is to boot.verb code
	for(int i=1; i<argc; ++i) {
		if(!strcmp(argv[i], "-stats")) {
			SHOW_RUN_STATS = true;
		}
		else if(!strcmp(argv[i], "-profile")) {
			DO_PROFILING = true;
			SHOW_RUN_STATS = true; // -profile implies -stats
		}
		else {
			cmdline_args.data.objlist->push_back(newString(argv[i]));
		}
	}
	
	// set path to bootfile -- require VERBII_BOOT to be set to avoid duplicating the
	// path functions that are in verbii code
	char bootfile[PATH_MAX];
	char *rootdir = getenv("VERBII_BOOT");
	if(!rootdir) {
		printf("* VERBII_BOOT must be set to the path where boot.verb.b is located.\n");
		exit(1);
	}
	strcpy(bootfile, rootdir);
	if(bootfile[strlen(bootfile)-1] != '/') {
		strcat(bootfile, "/");
	}
	strcat(bootfile, "boot.verb.b");
	//printf("** BOOTFILE: %s\n", bootfile);

	Interpreter *intr = NULL;
	while(1) {
		try {
			intr = new Interpreter();
			intr->PROFILE_CALLS = DO_PROFILING;
			// boot.verb expects cmdline args on top of stack on entry
			intr->push(cmdline_args);
			//printf("STACK BEFORE BOOT: %s\n", intr->reprStack().c_str());
			deserialize_and_run(intr, bootfile);
			if(SHOW_RUN_STATS)
				intr->print_stats();
			break;
		}
		catch (LangError &err) {
			auto errstr = "*** " + string(err.what()) + " ***";
			if(STACKTRACE_ON_EXCEPTION)
				print_backtrace(intr);
				
			printf("%s\n", errstr.c_str());
			// see if boot.verb requested to exit on exception or run again
			if(EXIT_ON_EXCEPTION)
				exit(1);
		}
	}
}

