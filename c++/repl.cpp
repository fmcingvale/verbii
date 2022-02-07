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
using namespace std;

string INITLIB = "../lib/init.txt";

string readfile(string filename) {
	ifstream fileIn(filename);
	string line, buf;
	while(getline(fileIn, line)) {
		buf += "\n" + line;
	}
	return buf;
}

Interpreter* newInterpreter(bool noinit) {
	auto intr = new Interpreter();
	
	// run initlib to load its words first, unless -noinit was given
	if(!noinit) {
		auto buf = readfile(INITLIB);
		intr->addText(buf);
		intr->run();
		// don't want initlib in the backtrace history, once it has successfully loaded
		intr->reader.clearAll();
		// also GC after loading large file
		x_mem_gcollect();
	}
	return intr;
}

void repl(bool noinit) {
	auto intr = newInterpreter(noinit);
	
	while(1) {
		printf(">> ");
		fflush(stdout);
		string line;
		getline(cin, line);
		if (line == "quit") {
			return;
		}
		//cout << "LINE: " << line << endl;
		intr->addText(line);

		intr->run();
		cout << "=> " << intr->reprStack() << endl;
	}
}

// like a non-interactive repl, reads a line at a time, prints it,
// runs it, then prints the stack. this is intented for unittesting.
// maxline is maximum line that ran OK last time so i ca restart.
void run_test_mode(string filename, bool noinit, int &maxrunline, bool &done) {

	//cout << "Test mode starting ... " << endl;
	done = false;

	regex blankline(R"""(^[ \t\r\n]*$)""");

	auto intr = newInterpreter(noinit);

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
		intr->reader.clearAll(); // ensure no leftover text from previous line
		intr->addText(line);
		intr->run();
		cout << "=> " << intr->reprStack() << endl;
		// update maxline only after the above runs ok
		maxrunline = runnable_lines;
	}
	done = true; // done
}

void run_file(Interpreter *intr, string filename, bool singlestep) {
	// run file
	auto buf = readfile(filename);
	intr->addText(buf);
	intr->run(singlestep);
}

void backtrace_curframe(Interpreter *intr) {
	string trace = "";
	int nr = 7; // number of words to print in each frame
	while(nr--) {
		auto w = intr->reader.prevWord();
		if(w == "") {
			cout << trace << endl;
			return;
		}
		else {
			trace = w + ' ' + trace;
		}
	}
	cout << trace << endl;
}

void print_backtrace(Interpreter *intr) {
	int i=0;
	while(1) {
		cout << "FRAME " << i++ << ": ";
		backtrace_curframe(intr);
		if(intr->reader.hasPushedWords()) {
			intr->reader.popWords();
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
	cout << "# WORDS: " << WORDS.size() << endl;
	cout << "# LAMBDAS: " << LAMBDAS.size() << endl;
}

#include <sys/stat.h>

bool file_exists(char* filename) {
	struct stat st;
	return stat(filename, &st) == 0;
}

int main(int argc, char *argv[]) {
	x_mem_init();

	bool testMode = false;
	string filename = "";
	bool noinit = false;
	bool gcstats = false;
	bool singlestep = false;
	for(int i=1; i<argc; ++i) {
		if(!strcmp(argv[i], "-test")) {
			testMode = true;
		}
		else if(!strcmp(argv[i], "-noinit")) {
			noinit = true;
		}
		else if(!strcmp(argv[i], "-showgc")) {
			gcstats = true;
		}
		else if(!strcmp(argv[i], "-step")) {
			singlestep = true;
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
				repl(noinit);
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
				run_test_mode(filename, noinit, maxrunline, done);
			}
			catch (LangError &err) {
				cout << "*** " << err.what() << " ***\n";
				++maxrunline; // skip line that failed last time
			}
		}
	}
	else {		
		auto intr = newInterpreter(noinit);

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

