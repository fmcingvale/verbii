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

string readfile(string filename) {
	ifstream fileIn(filename);
	string line, buf;
	while(getline(fileIn, line)) {
		buf += "\n" + line;
	}
	return buf;
}

Interpreter* newInterpreter() {
	auto intr = new Interpreter();
	
	//cout << "Starting interpreter ..." << endl;

	// load byte-compiled init.verb and compiler.verb to bootstrap interpreter
	ifstream fileIn(INITLIB);
	deserialize_stream(intr, fileIn);
	// run __main__ in initlib to setup any globals
	auto code = intr->lookup_word("__main__");
	intr->run(code);
	fileIn = ifstream(COMPILERLIB);
	deserialize_stream(intr, fileIn);
	// do NOT run compiler __main__ since that is used for compiling from the cmdline

	// delete __main__ now so I don't inadvertently run it again -- i.e. a later byte-compilation
	// might fail, but leaving and older __main__ in place
	intr->deleteWord("__main__");

	// GC after loading large file
	x_mem_gcollect();

	return intr;
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
void compile_and_run(Interpreter *intr, string text, bool singlestep) {
	// push string, then call byte-compile-string
	intr->push(newString(text));
	auto code = intr->lookup_word("byte-compile-string");
	intr->run(code);

	// byte-compile-string leaves list of words on stack -- used by serializer -- but i
	// don't need them here
	intr->pop();

	// run __main__
	code = intr->lookup_word("__main__");

	if(singlestep)
		intr->run(code, &debug_hook);
	else
		intr->run(code);

	// like above, delete __main__ after running
	intr->deleteWord("__main__");
}
	
void backtrace_curframe(Interpreter *intr) {
	string trace = "";
	int nr = 7; // number of words to print in each frame
	while(nr--) {
		auto o = intr->prevCodeObj();
		if(o.isNull()) {
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
		if(intr->callstack_code.size() > 0) {
			intr->code_return();
		}
		else {
			return;
		}
	}
}

// return empty string if OK, error message on error
string safe_compile_and_run(Interpreter *intr, string text, bool singlestep, bool backtrace_on_error) {
	try {
		compile_and_run(intr, text, singlestep);
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
	auto errmsg = safe_compile_and_run(newInterpreter(), readfile(filename), singlestep, true);
	if(errmsg.length() > 0) {
		cout << errmsg << endl;
	}
}

void print_gc_stats() {
#if defined(USE_GCMALLOC)
	GC_word pheap_size, pfree_bytes, punmapped_bytes, pbytes_since_gc, ptotal_bytes;
	GC_get_heap_usage_safe(&pheap_size, &pfree_bytes, &punmapped_bytes, &pbytes_since_gc, &ptotal_bytes);
	cout << "Heap size: " << pheap_size << endl;
	cout << "Free bytes: " << pfree_bytes << endl;
	cout << "Unmapped bytes: " << punmapped_bytes << endl;
	cout << "Bytes since gc: " << pbytes_since_gc << endl;
	cout << "Total bytes: " << ptotal_bytes << endl;
#else
	cout << "xmalloc bytes: " << X_BYTES_ALLOCATED << endl;
#endif
	cout << "# BUILTINS: " << BUILTINS.size() << endl;
}

#include <sys/stat.h>

bool file_exists(char* filename) {
	struct stat st;
	return stat(filename, &st) == 0;
}

#include "native.hpp"

int main(int argc, char *argv[]) {
	x_mem_init();

	bool testMode = false;
	string filename = "";
	bool gcstats = false;
	bool singlestep = false;

	native_cmdline_args = newList();

	for(int i=1; i<argc; ++i) {
		if(!strcmp(argv[i], "-test")) {
			testMode = true;
		}
		else if(!strcmp(argv[i], "-showgc")) {
			gcstats = true;
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
	if(gcstats) {
		print_gc_stats();
		cout << "size of Object: " << sizeof(Object) << endl;
	}
	return 0;
}

