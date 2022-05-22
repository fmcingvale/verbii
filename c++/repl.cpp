/*
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "interpreter.hpp"
#include "errors.hpp"
#include <iostream>
#include <fstream>
#include <regex>
#include "xmalloc.hpp"
#include "native.hpp"
#include "deserialize.hpp"
using namespace std;

string INITLIB = "../lib/init.verb.b";
string COMPILERLIB = "../lib/compiler.verb.b";
string PATCHESLIB = "../lib/patches.verb";

bool SHOW_RUN_STATS = false;

void print_backtrace(Interpreter *intr);

string readfile(string filename) {
	ifstream fileIn(filename);
	string line, buf;
	while(getline(fileIn, line)) {
		buf += "\n" + line;
	}
	return buf;
}

void deserialize_and_run(Interpreter *intr, string filename) {
	ifstream fileIn(filename);
	deserialize_stream(intr, fileIn);
	// run __main__ to setup any vars
	auto code = intr->lookup_word("__main__");
	intr->run(code);
	// always delete __main__, else next file will fail to load
	intr->deleteWord("__main__");
}

void compile_and_load(Interpreter *intr, string &text, bool allowOverwrite) {
	// set flag so make-word can overwrite existing words
	ALLOW_OVERWRITING_WORDS = allowOverwrite;
	// normal implementation -- see below if errors are happening in the compiler
	#if 1
	intr->push(newString(text));
	auto code = intr->lookup_word("compile-and-load-string");
	intr->run(code);
	#endif
	// normally don't want to catch errors here, better to catch them later,
	// but sometimes the compiler breaks so badly it's helpful to turn this on
	// temporarily
	#if 0
	try {
		intr->push(newString(text));
		auto code = intr->lookup_word("compile-and-load-string");
		intr->run(code);
	}
	catch (LangError &err) {
		auto errstr = "*** " + string(err.what()) + " ***";
		printf("%s\n", errstr.c_str());
		print_backtrace(intr);
		exit(1);
	}
	#endif
	// turn flag back off (default)
	ALLOW_OVERWRITING_WORDS = false;
}

void debug_hook(Interpreter *intr, Object obj) {
	std::cout << "=> " << intr->reprStack() << endl;
	std::cout << "Run: " << obj.fmtStackPrint() << endl;
	cout << "press ENTER to continue ...";
	fflush(stdout);
	string line;
	getline(cin, line);
}

// use safe_ version below
void compile_and_run(Interpreter *intr, string text, bool singlestep, bool allowOverwrite=false) {
	compile_and_load(intr, text, allowOverwrite);
	
	// run __main__
	auto code = intr->lookup_word("__main__");

	// subtlety -- the code i'm about to run might want to redefine __main__
	// (i.e. if i'm running the compiler). so delete __main__ BEFORE running
	intr->deleteWord("__main__");

	if(singlestep)
		intr->run(code, &debug_hook);
	else
		intr->run(code);
}
	
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

// return empty string if OK, error message on error
string safe_compile_and_run(Interpreter *intr, string text, bool singlestep, bool backtrace_on_error,
							bool allowOverwrite=false) {
	try {
		compile_and_run(intr, text, singlestep, allowOverwrite);
		return "";
	}
	catch (LangError &err) {
		auto errstr = "*** " + string(err.what()) + " ***";
		if(backtrace_on_error) {
			print_backtrace(intr);
		}
		return errstr;
	}
}

Interpreter* newInterpreter() {
	auto intr = new Interpreter();
	
	//cout << "Starting interpreter ..." << endl;

	// load byte-compiled init.verb and compiler.verb to bootstrap interpreter
	deserialize_and_run(intr, INITLIB);
	deserialize_and_run(intr, COMPILERLIB);

	// now that those are loaded, load & run the patches file like any other
	// source file (this is the only file that is allowed to overwrite existing words)
	safe_compile_and_run(intr, readfile(PATCHESLIB), false, true, true);
	
	// GC after loading init files
	x_mem_gcollect();

	return intr;
}

void repl(bool singlestep) {
	cout << "Verbii compiled with g++ " << __GNUC__ << "." << __GNUC_MINOR__ << "." << __GNUC_PATCHLEVEL__ << endl;
	auto intr = newInterpreter();
	
	while(1) {
		printf(">> ");
		fflush(stdout);
		string line;
		if(!getline(cin, line)) {
			return;
		}
		if (line == "quit" || line == ",q") {
			if(SHOW_RUN_STATS)
				intr->print_stats();
			return;
		}
		auto errmsg = safe_compile_and_run(intr, line, singlestep, true);
		if(errmsg.length() > 0) {
			cout << errmsg << endl;
			intr = newInterpreter(); // restart interpreter on error
		}
		else {
			std::cout << "=> " << intr->reprStack() << endl;
		}
	}
}

// like a non-interactive repl, reads a line at a time, prints it,
// runs it, then prints the stack. this is intented for unittesting.
// maxline is maximum line that ran OK last time so i ca restart.
void run_test_mode(string filename) {

	regex blankline(R"""(^[ \t\r\n]*$)""");

	auto intr = newInterpreter();

	string line;
	ifstream fileIn(filename);
	
	while(getline(fileIn, line)) {
		// skip blank lines
		if(regex_match(line, blankline)) {
			continue;
		}
	
		cout << ">> " << line << endl;
		// no backtraces here - if an unexpected error occurs, rerun test case without -test
		// to see the backtrace
		auto errmsg = safe_compile_and_run(intr, line, false, false);
		if(errmsg.length() > 0) {
			cout << errmsg << endl;
			intr = newInterpreter(); // restart on error
		}
		else {
			cout << "=> " << intr->reprStack() << endl;
		}
	}
}

void run_file(string filename, bool singlestep) {
	auto intr = newInterpreter();
	auto errmsg = safe_compile_and_run(intr, readfile(filename), singlestep, true);
	if(errmsg.length() > 0) {
		cout << errmsg << endl;
	}
	else {
		if(SHOW_RUN_STATS)
			intr->print_stats();
	}
}

#include <sys/stat.h>

bool file_exists(char* filename) {
	struct stat st;
	return stat(filename, &st) == 0;
}

#include "native.hpp"

int main(int argc, char *argv[]) {
	x_mem_init();

	// measure absolute time from time I start running
	STARTUP_TIME = chrono::steady_clock::now();

	bool testMode = false;
	string filename = "";
	bool singlestep = false;

	native_cmdline_args = newList();

	for(int i=1; i<argc; ++i) {
		if(!strcmp(argv[i], "-test")) {
			testMode = true;
		}
		else if(!strcmp(argv[i], "-stats")) {
			SHOW_RUN_STATS = true;
		}
		else if(!strcmp(argv[i], "-step")) {
			singlestep = true;
		}
		else if(!strcmp(argv[i], "--")) {
			// pass rest of args to script
			while(++i < argc) {
				native_cmdline_args.data.objlist->push_back(newString(argv[i]));
			}
			break;
		}
		else if(filename == "" && file_exists(argv[i])) {
			filename = argv[i];
		}
		else {
			cout << "Unknown argument: " << argv[i] << endl;
			exit(1);
		}
	}

	if(filename == "")
		repl(singlestep);
	else if(testMode)
		run_test_mode(filename);
	else
		run_file(filename, singlestep);

	//GC_gcollect();
	return 0;
}

