
#include "interpreter.hpp"
#include <readline/readline.h>
#include <iostream>
#include <filesystem>
#include <fstream>
using namespace std;
namespace fs = std::filesystem;

void repl() {
	auto intr = new Interpreter();
	
	while(1) {
		printf(">> ");
		fflush(stdout);
		string line;
		getline(cin, line);
		intr->addText(line);
		intr->run();
		cout << "=> " << intr->reprStack() << endl;
	}
}

// like a non-interactive repl, reads a line at a time, prints it,
// runs it, then prints the stack. this is intented for unittesting.
void run_test_mode(string filename) {

	auto intr = new Interpreter();
	string line;
	ifstream fileIn(filename);
	
	while(getline(fileIn, line)) {
		cout << ">> " << line << endl;
		intr->addText(line);
		intr->run();
		cout << "=> " << intr->reprStack() << endl;
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

