/*
	Deserialize - load bytecode produced by compiler.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "deserialize.hpp"
#include "errors.hpp"

using namespace std;

static void string_replace(string &s, const char *from, const char *to) {
	size_t i;
	while((i = s.find(from)) != string::npos)
		s.replace(i, strlen(from), to);
}

// FYI -- stream from compiler will be a single list containing word definitions

// see compiler.verb:serialize-object, but briefly, I only have to deserialize the
// format that the compiler produces, not ANY arbitrary verbii object

// deserialize & return next object from stream
Object deserialize_stream(Interpreter *intr, ifstream &fileIn) {
	string line;
	if(getline(fileIn, line)) {
		switch(line[0]) {
			case 'i': return parseInt(line.substr(2));
			case 'f': return parseFloat(line.substr(2));
			case 'b': return parseBool(line.substr(2));
			case 'n': return newNull();
			case 'o': return newOpcode(parseInt(line.substr(2)).asInt());
			case 's':
				string_replace(line, "%32", " ");
				string_replace(line, "%09", "\t");
				string_replace(line, "%10", "\n");
				string_replace(line, "%13", "\r");
				string_replace(line, "%37", "%");
				return newString(line.substr(2));
			case 'y': return newSymbol(line.substr(2));
			case 'L': // list
				{
					int nr = parseInt(line.substr(2)).asInt();
					Object list = newList();
					for(int i=0; i<nr; ++i)
						list.data.objlist->push_back(deserialize_stream(intr,fileIn));
					return list;
				}
			case 'F': // lambda
				{
					Object list = deserialize_stream(intr,fileIn);
					if(!list.isList())
						throw LangError("Expecting list after F but got:" + list.fmtStackPrint());
					return newLambda(list.data.objlist);
				}
			case 'W': // word definition
				{
					string name = line.substr(2);
					Object list = deserialize_stream(intr,fileIn);
					if(!list.isList())
						throw LangError("Expecting list after W but got:" + list.fmtStackPrint());

					// insert into interpreter
					//cout << "Inserting word:>>>" << name << "<<<" << endl;
					//intr->WORDS[name.c_str()] = list.data.objlist;

					// do not allow overwriting words when deserializing
					intr->defineWord(name.c_str(), list.data.objlist, false);
					// this produces nothing extra
					return newVoid();
				}

			default:
				// this is a fatal error, so just print & exit -- trying to do an exception
				// makes the output weird
				//throw LangError("Unrecogized line in deserialize:" + line);
				printf("Unrecogized line in deserialize: %s\n", line.c_str());
				exit(1);
		}
	}
	return newVoid();
}
	