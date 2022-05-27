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

bool SHOW_RUN_STATS = false; // -stats
bool NO_CACHE_COMPILATION = false; // -nocache

void print_backtrace(Interpreter *intr);

string readfile(string& filename) {
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
	if(!code) 
		throw LangError("Unable to find __main__ after deserializing: " + filename);

	intr->run(code);
	// always delete __main__, else next file will fail to load
	//printf("DEL MAIN 1\n");
	intr->deleteWord("__main__");
}

// returns error message or "" on no error
string compile_and_load_string(Interpreter *intr, string &text, bool allowOverwrite) {
	// set flag so make-word can overwrite existing words when requested
	ALLOW_OVERWRITING_WORDS = allowOverwrite;
	// normal implementation -- see below if errors are happening in the compiler
	#if 0
	intr->push(newString(text));
	auto code = intr->lookup_word("compile-and-load-string");
	if(!code)
		throw LangError("Unable to find compile-and-load-string");

	intr->run(code);
	#endif
	// normally don't want to catch errors here, better to catch them later,
	// but sometimes the compiler breaks so badly it's helpful to turn this on
	// temporarily
	#if 1
	try {
		intr->push(newString(text));
		auto code = intr->lookup_word("compile-and-load-string");
		if(!code)
			throw LangError("Unable to find compile-and-load-string");

		intr->run(code);
		return "";
	}
	catch (LangError &err) {
		auto errstr = "*** " + string(err.what()) + " ***";
		// don't normally want backtraces on compilation errors since the compilation error
		// will say what happened. but sometimes i break things really badly and need to turn
		// this on ... (turning it on normally will break unittests)
		//print_backtrace(intr);
		return errstr;
	}
	#endif
	// turn flag back off (default)
	ALLOW_OVERWRITING_WORDS = false;
}

// load filename with caching of bytecode to/from filename.b
string cached_compile_and_load_file(Interpreter *intr, string &filename, bool allowOverwrite) {
	if(!NO_CACHE_COMPILATION) {
		// normal caching version

		// set flag so make-word can overwrite existing words when requested
		ALLOW_OVERWRITING_WORDS = allowOverwrite;
		intr->push(newString(filename));
		// catch compilation errors
		try {
			auto code = intr->lookup_word("cached-compile-and-load");
			if(!code)
				throw LangError("Unable to find cached-compile-and-load");

			intr->run(code);			
		}
		catch (LangError &err) {
			auto errstr = "*** " + string(err.what()) + " ***";
			// don't normally want backtraces on compilation errors since the compilation error
			// will say what happened. but sometimes i break things really badly and need to turn
			// this on ... (turning it on normally will break unittests)
			//print_backtrace(intr);
			return errstr;
		}
		// turn flag back off (default)
		ALLOW_OVERWRITING_WORDS = false;
		return ""; // no error
	}
	else {
		// caching disabled
		string text = readfile(filename);
		return compile_and_load_string(intr, text, allowOverwrite);
	}
}

void debug_hook(Interpreter *intr, Object obj) {
	std::cout << "=> " << intr->reprStack() << endl;
	std::cout << "Run: " << obj.fmtStackPrint() << endl;
	cout << "press ENTER to continue ...";
	fflush(stdout);
	string line;
	getline(cin, line);
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

// you must call compile_and_load_string() or cached_compile_and_load_file() before
// calling this to load code into the passed interpreter
//
// __main__ will have been deleted upon exit from this function
//
// return empty string if OK, error message on error
string safe_run_main(Interpreter *intr, bool singlestep, bool backtrace_on_error) {
	try {
		// run __main__
		auto code = intr->lookup_word("__main__");
		if(!code)
			throw LangError("Unable to find __main__ in safe_run_main()");

		// subtlety -- the code i'm about to run might want to redefine __main__
		// (i.e. if i'm running the compiler). so delete __main__ BEFORE running
		//printf("DEL MAIN 2\n");
		intr->deleteWord("__main__");

		if(singlestep)
			intr->run(code, &debug_hook);
		else
			intr->run(code);
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
	// source file, except it is allowed to overwrite existing words

	// FIX for now don't cache patches file
	//string text = readfile(PATCHESLIB);
	//compile_and_load_string(intr,text,true);
	
	auto errmsg = cached_compile_and_load_file(intr, PATCHESLIB, true);
	if(errmsg.length() >0) {
		cout << errmsg << endl;
		// can't continue if patches didn't load OK
		exit(1);		
	}

	errmsg = safe_run_main(intr, false, true);
	if(errmsg.length() >0) {
		cout << errmsg << endl;
		// can't continue if patches didn't load OK
		exit(1);		
	}

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
		// compile & run line
		auto errmsg = compile_and_load_string(intr, line, false);
		if(errmsg.length() >0) {
			cout << errmsg << endl;
			intr = newInterpreter(); // restart interpreter on error
			continue;
		}
		errmsg = safe_run_main(intr, singlestep, true);
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
void run_test_mode(string& filename) {

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
		// compile & load line
		auto errmsg = compile_and_load_string(intr, line, false);
		if(errmsg.length() > 0) {
			cout << errmsg << endl;
			intr = newInterpreter(); // restart on error
			continue;
		}
		// no backtraces here - if an unexpected error occurs, rerun test case without -test
		// in order to see the backtrace
		errmsg = safe_run_main(intr, false, false);
		if(errmsg.length() > 0) {
			cout << errmsg << endl;
			intr = newInterpreter(); // restart on error
		}
		else {
			cout << "=> " << intr->reprStack() << endl;
		}
	}
}

void run_file(string& filename, bool singlestep) {
	auto intr = newInterpreter();
	// compile & load
	auto errmsg = cached_compile_and_load_file(intr, filename, false);
	if(errmsg.length() > 0) {
		cout << errmsg << endl;
		return;
	}
	// run with backtrace on error
	errmsg = safe_run_main(intr, singlestep, true);
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
		else if(!strcmp(argv[i], "-nocache")) {
			NO_CACHE_COMPILATION = true;
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

