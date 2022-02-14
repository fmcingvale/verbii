/*
	Reader - splits text into words and provides next/previous interface.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "xmalloc.hpp"
#include "reader.hpp"
#include "errors.hpp"
#include <iostream>
#include <sstream>

using namespace std;

Reader::Reader() {
	objlist = new ObjList();
	pos = 0;
}

// add more text to current context
void Reader::addText(const string &text) {
	istringstream input(text);
	string word;
	while(true) {
		input >> word; // read words, delimited by whitespace
		if(input.fail()) {
			return; // reached end of file (probably some whitespace was left, so failed to read a word)
		}
		objlist->push_back(newSymbol(word.c_str(), word.length()));
	}
}

void Reader::clearAll() {
	stack_objlists.clear();
	stack_pos.clear();
	delete objlist;
	objlist = new ObjList();
	pos = 0;
}

// push current context and switch to new context
void Reader::pushObjList(ObjList *objs) {
	stack_objlists.push_back(objlist);
	stack_pos.push_back(pos);
	
	objlist = objs;
	pos = 0;
}

void Reader::popObjList() {
	objlist = stack_objlists.back();
	pos = stack_pos.back();
	stack_objlists.pop_back();
	stack_pos.pop_back();
}

bool Reader::hasPushedObjLists() {
	return stack_objlists.size() > 0;
}

const Object Reader::nextObj() {
	if(pos >= objlist->size()) {
		return NULLOBJ;
	}
	else {
		return objlist->at(pos++);
	}
}

const Object Reader::peekObj() {
	if(pos >= objlist->size()) {
		return NULLOBJ;
	}
	else {
		return objlist->at(pos);
	}
}

const Object Reader::prevObj() {
	if(pos <= 0) {
		return NULLOBJ;
	}
	else {
		return objlist->at(--pos);
	}
}

const Object Reader::peekPrevObj() {
	if(pos <= 0) {
		return NULLOBJ;
	}
	else {
		return objlist->at(pos-1);
	}
}

void Reader::deletePrevObj() {
	if(pos == 0) {
		throw LangError("No previous word to delete!");
	}
	objlist->erase(objlist->begin()+pos-1);
	--pos;
}

void Reader::insertPrevObj(const Object& obj) {
	objlist->insert(objlist->begin()+pos, obj);
	++pos;
}

void Reader::debugPrintObjList() const {
	cout << "Reader:objlist>> ";
	for(auto obj : *objlist) {
		cout << obj.fmtStackPrint() << " ";
	}
	cout << endl;
}


