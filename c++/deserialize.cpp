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

// stream from compiler will be a single list containing word definitions

Object deserialize_stream(Interpreter *intr, ifstream &fileIn) {
	string line;
	if(getline(fileIn, line)) {
		switch(line[0]) {
			case 'i': return parseInt(line.substr(2));
			case 'f': return parseFloat(line.substr(2));
			case 'n': return newNull();
			case 'b': return (line.substr(2) == "true") ? newBool(true) : newBool(false);
			case 's':
				string_replace(line, "%32", " ");
				string_replace(line, "%10", "\n");
				string_replace(line, "%13", "\r");
				string_replace(line, "%%", "%");
				return newString(line.substr(2));
			case 'y': return newSymbol(line.substr(2));
			case 'L':
				{
					int nr = parseInt(line.substr(2)).asInt();
					Object list = newList();
					for(int i=0; i<nr; ++i)
						list.data.objlist->push_back(deserialize_stream(intr,fileIn));
					return list;
				}
			case 'F':
				{
					Object list = deserialize_stream(intr,fileIn);
					if(!list.isList())
						throw LangError("Expecting list but got:" + list.fmtStackPrint());
					return newLambda(list.data.objlist);
				}
			case 'W':
				{
					string name = line.substr(2);
					Object list = deserialize_stream(intr,fileIn);
					if(!list.isList())
						throw LangError("Expecting list after W but got:" + list.fmtStackPrint());

					// insert into interpreter
					//cout << "Inserting word:>>>" << name << "<<<" << endl;
					intr->WORDS[name.c_str()] = list.data.objlist;
					// this produces nothing extra
					return newVoid();
				}

			default:
				throw LangError("Unrecogized line in deserialize:" + line);
		}
	}
	return newVoid();
}
 

	
			