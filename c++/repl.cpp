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

void repl() {
	auto intr = make_unique<Interpreter>();
	//auto intr = new Interpreter();
	
	while(1) {
		printf(">> ");
		fflush(stdout);
		string line;
		getline(cin, line);
		if (line == "quit") {
			return;
		}
		intr->addText(line);
		try {
			intr->run();
			// assume reprStack could throw as well
			cout << "=> " << intr->reprStack() << endl;
		}
		catch (LangError &err) {
			cout << "*** " << err.what() << " ***\n";
		}
	}
}

// like a non-interactive repl, reads a line at a time, prints it,
// runs it, then prints the stack. this is intented for unittesting.
void run_test_mode(string filename) {

	regex blankline(R"""(^[ \t\r\n]*$)""");

	auto intr = new Interpreter();
	string line;
	ifstream fileIn(filename);
	
	while(getline(fileIn, line)) {
		// skip blank lines
		if(regex_match(line, blankline)) {
			continue;
		}
		cout << ">> " << line << endl;
		intr->addText(line);
		try {
			intr->run();
			// assume reprStack could throw as well
			cout << "=> " << intr->reprStack() << endl;
		}
		catch (LangError &err) {
			cout << "*** " << err.what() << " ***\n";
		}
	}
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
		repl();
	}
	else if(testMode) {
		run_test_mode(filename);
	}
	#if 0
	else {
		run_file(filename);
	}
	#endif
	return 0;
}

