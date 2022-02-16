
#pragma once

#include "langtypes.hpp"
#include "reader.hpp"

class Syntax {
	public:
	Syntax() {}

	// mirrored from the Reader interface. only Syntax is allowed to
	// rewrite the object lists (delete/insertPrevObject)

	void addText(const std::string &text) { reader.addText(text); }
	void clearAll() { reader.clearAll(); }
	void pushObjList(ObjList *objs) { reader.pushObjList(objs); }
	void popObjList() { reader.popObjList(); }
	bool hasPushedObjLists() { return reader.hasPushedObjLists(); }

	const Object nextObj();
	const Object peekObj();
	const Object prevObj();
	const Object peekPrevObj();

	// to avoid a lot of obj.isNull() checks, these require a non-empty
	// word or they throw an exception ... for use in cases where there MUST
	// be a next/previous word, or its a syntax error
	Object nextObjOrFail();
	Object prevObjOrFail();
	// or to require a symbol
	Object nextSymbolOrFail();

	protected:
	Reader reader;
	Object parse_lambda();
};

