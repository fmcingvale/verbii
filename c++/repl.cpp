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

	// GC after loading large file
	x_mem_gcollect();

	return intr;
}

void repl() {
	auto intr = newInterpreter();
	
	while(1) {
		printf(">> ");
		fflush(stdout);
		string line;
		if(!getline(cin, line)) {
			return;
		}
		if (line == "quit") {
			return;
		}
		//cout << "LINE: " << line << endl;
		//intr->addText(line);

		// push string, then call byte-compile-string
		intr->push(newString(line));
		auto code = intr->lookup_word("byte-compile-string");
		intr->run(code);

		// byte-compile-string leaves list of words on stack -- used by serializer -- but i
		// don't need them here
		intr->pop();

		// run __main__
		code = intr->lookup_word("__main__");

		intr->run(code);
		cout << "=> " << intr->reprStack() << endl;
	}
}

// like a non-interactive repl, reads a line at a time, prints it,
// runs it, then prints the stack. this is intented for unittesting.
// maxline is maximum line that ran OK last time so i ca restart.
void run_test_mode(string filename, int &maxrunline, bool &done) {

	//cout << "Test mode starting ... " << endl;
	done = false;

	regex blankline(R"""(^[ \t\r\n]*$)""");

	auto intr = newInterpreter();

	string line;
	ifstream fileIn(filename);
	
	int runnable_lines = 0; // how many runnable lines have i seen (i.e. not counting blank lines)
	while(getline(fileIn, line)) {
		// skip blank lines
		if(regex_match(line, blankline)) {
			continue;
		}
		++runnable_lines;
		//cout << "On runnable line# " << runnable_lines << ", maxrunline " << maxrunline << endl;

		if(runnable_lines <= maxrunline) {
			//cout << "Skipping line ... " << endl;
			// counts as running, since if i fail i want to restart at the NEXT line
			maxrunline = max(maxrunline,runnable_lines);
			continue;
		}
		cout << ">> " << line << endl;
		//intr->syntax->clearAll(); // ensure no leftover text from previous line
		//intr->addText(line);
		// like above, compile and run line
		intr->push(newString(line));
		auto code = intr->lookup_word("byte-compile-string");
		intr->run(code);
		intr->pop(); // like above, pop list of compiled words

		code = intr->lookup_word("__main__");
		intr->run(code);

		cout << "=> " << intr->reprStack() << endl;
		// update maxline only after the above runs ok
		maxrunline = runnable_lines;
	}
	done = true; // done
}

void run_file(Interpreter *intr, string filename, bool singlestep) {
	// run file
	auto buf = readfile(filename);
	// as above, compile then run
	intr->push(newString(buf));
	auto code = intr->lookup_word("byte-compile-string");
	intr->run(code);
	intr->pop(); // like above, pop list of compiled words

	code = intr->lookup_word("__main__");
	intr->run(code, singlestep);
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

	if(filename == "") {
		bool exited = false;
		while(!exited) {
			// restart repl on exceptions - continue until it exits OK.
			// (continuing after the exception gives weird errors so something
			// is getting corrupted in the interpreter)
			try {
				repl();
				exited = true;
			}
			catch (LangError &err) {
				cout << "*** " << err.what() << " ***\n";
			}
		}
	}
	else if(testMode) {
		int maxrunline = 0;
		bool done = false;
		while(!done) {
			// when exception occurs, need to restart interpreter, or
			// weird errors happen. track max line that i ran before so
			// it restarts on next line
			try {
				run_test_mode(filename, maxrunline, done);
			}
			catch (LangError &err) {
				cout << "*** " << err.what() << " ***\n";
				++maxrunline; // skip line that failed last time
			}
		}
	}
	else {		
		auto intr = newInterpreter();

		try {
			run_file(intr, filename, singlestep);
		}
		catch (LangError &err) {
			cout << "*** " << err.what() << " ***\n";
			print_backtrace(intr);
		}
	}

	//GC_gcollect();
	if(gcstats) {
		print_gc_stats();
		cout << "size of Object: " << sizeof(Object) << endl;
	}
	return 0;
}

