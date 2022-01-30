/*
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "interpreter.hpp"
#include "errors.hpp"
#include <readline/readline.h>
#include <iostream>
#include <filesystem>
#include <fstream>
#include <regex>
using namespace std;
namespace fs = std::filesystem;

string readfile(string filename) {
	ifstream fileIn(filename);
	string line, buf;
	while(getline(fileIn, line)) {
		buf += "\n" + line;
	}
	return buf;
}

void repl() {
	auto intr = make_unique<Interpreter>();
	
	// run initlib to load its words first
	auto buf = readfile("initlib.txt");
	intr->addText(buf);
	intr->run();
	
	while(1) {
		printf(">> ");
		fflush(stdout);
		string line;
		getline(cin, line);
		if (line == "quit") {
			return;
		}
		cout << "LINE: " << line << endl;
		intr->addText(line);

		intr->run();
		cout << "=> " << intr->reprStack() << endl;
	}
}

// like a non-interactive repl, reads a line at a time, prints it,
// runs it, then prints the stack. this is intented for unittesting.
// maxline is maximum line that ran OK last time so i ca restart.
void run_test_mode(string filename, int &maxline, bool &done) {

	done = false;

	regex blankline(R"""(^[ \t\r\n]*$)""");

	auto intr = new Interpreter();

	// run initlib to load its words first
	auto buf = readfile("initlib.txt");
	intr->addText(buf);
	intr->run();
	
	string line;
	ifstream fileIn(filename);
	
	int lines_read = 0;
	while(getline(fileIn, line)) {
		++lines_read;
		// skip blank lines
		if(regex_match(line, blankline)) {
			continue;
		}
		if(lines_read <= maxline) {
			continue;
		}
		cout << ">> " << line << endl;
		intr->addText(line);
		intr->run();
		cout << "=> " << intr->reprStack() << endl;
		// update maxline only after the above runs ok
		maxline = lines_read;
	}
	done = true; // done
}

int main(int argc, char *argv[]) {
	bool testMode = false;
	string filename = "";
	for(int i=1; i<argc; ++i) {
		if(!strcmp(argv[i], "-test")) {
			testMode = true;
		}
		else if(fs::exists(argv[i])) {
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
		int maxline = 0;
		bool done = false;
		while(!done) {
			// when exception occurs, need to restart interpreter, or
			// weird errors happen. track max line that i ran before so
			// it restarts on next line
			try {
				run_test_mode(filename, maxline, done);
			}
			catch (LangError &err) {
				cout << "*** " << err.what() << " ***\n";
				++maxline; // skip line that failed last time
			}
		}
	}
	#if 0
	else {
		run_file(filename);
	}
	#endif
	return 0;
}

