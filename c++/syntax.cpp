#include "syntax.hpp"
#include "errors.hpp"
#include <string>
#include <iostream>
using namespace std;

const Object Syntax::nextObj() {
	auto obj = reader.nextObj();
	// NOTE - i was originally using c++ regexes here to check for
	// integers and lambdas. after profiling and discovering they
	// were the using most of the program runtime, i changed to parsing
	// them myself. WITH regexes, runtime was 4.5x higher and memory
	// usage was 1500x (!!) larger (see commit [556839e] for the regex version)
	if(obj.isSymbol()) {
		{
			// see if it's an integer
			bool has_digits = false;
			const char *s = obj.asSymbol();
			if(*s == '+' || *s == '-') 
				++s;

			while(isdigit(*s)) {
				has_digits = true;
				++s;
			}

			// integers are just pushed to stack
			if(!*s && has_digits) {
				//cout << "MATCHED INT:" << obj.fmtStackPrint() << endl;
				auto intobj = newInt(stoi(obj.asSymbol()));
				reader.deletePrevObj();
				reader.insertPrevObj(intobj);
				return intobj;
			}
		}

		{
			// to keep parsing simple (i.e. should be easy to do without regexes),
			// floats are written like #n.nnn
			if(obj.asSymbol()[0] == '#') {
				auto fobj = newFloat(stod(obj.asSymbol()+1));
				reader.deletePrevObj();
				reader.insertPrevObj(fobj);
				return fobj;
			}
		}

		{
			if(obj.isSymbol("{")) {
				return parse_lambda();
			}
		}
	}

	return obj;
}

Object Syntax::nextObjOrFail() {
	if(peekObj().isNull()) {
		throw LangError("Unexpected end of input");
	}
	return nextObj();
}

Object Syntax::prevObjOrFail() {
	if(peekPrevObj().isNull()) {
		throw LangError("Unable to find previous word");
	}
	return prevObj();
}

Object Syntax::nextSymbolOrFail() {
	auto obj = nextObjOrFail();
	if(!obj.isSymbol()) {
		throw LangError("Expecting symbol, got: " + obj.fmtStackPrint());
	}
	return obj;
}

Object Syntax::parse_lambda() {
	// turn { ... } into an anonymous wordlist
	//
	// the FIRST time I see { .. }, create a tagged wordlist,
	// REWRITE the { ... } into the tagged value and push the tagged
	// value to the stack.
	//
	// each SUBSEQUENT time the same code runs, the tagged value will be
	// in the wordlist, so will be pushed to the stack.
	//
	// from the perspective of the user, the same thing happened both times --
	// the tagged lambda was pushed to the stack, ready to be called, stored, etc.
	
	// { was just read -- read the words until }
	
	// delete the { that was just read
	reader.deletePrevObj();

	auto objlist = new ObjList();
	int nesting = 1;
	while(true) {
		auto obj = nextObjOrFail();
		// delete the { ... } as I read it -- will replace it with a tagged wordlist
		reader.deletePrevObj();
		
		if(obj.isSymbol("{")) {
			// if I find inner lambdas, just copy them for now and later when they are run, 
			// this same process will happen for them
			++nesting;
			objlist->push_back(obj);
		}
		else if(obj.isSymbol("}")) {
			if(--nesting > 0) {
				objlist->push_back(obj);
				continue;
			}
			
			// replace { .. } in source wordlist with Lambda so it only has to be parsed once
			auto _lambda = newLambda(objlist);
			reader.insertPrevObj(_lambda);
			// next time, the interpreter will see the Lambda itself, so will not come back here
			return _lambda;
		}
		else {
			objlist->push_back(obj);
		}
	}
}

const Object Syntax::peekObj() {
	// I need the above processing to occur on peeked objects,
	// so do it this way ....
	auto obj = nextObj();
	if(!obj.isNull()) {
		reader.prevObj(); // rewind
	}
	return obj;
}

const Object Syntax::prevObj() {
	return reader.prevObj();
}

const Object Syntax::peekPrevObj() {
	return reader.peekPrevObj();
}
