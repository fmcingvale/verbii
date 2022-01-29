
#include "interpreter.hpp"
#include <readline/readline.h>
#include <iostream>
using namespace std;

int main(int argc, char *argv[]) {
	cout << "HELLO!\n";
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
	
return 0;
}

