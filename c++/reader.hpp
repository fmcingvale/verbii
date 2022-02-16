/*
	Reader - splits text into Objects and provides next/previous interface.
			The objects will mostly be Symbols but Objects are used so the
			caller can insert any kind of Object back into the stream.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once
#include <string>
#include <vector>
#include <tuple>
#include <memory>
#include "xmalloc.hpp"
#include "langtypes.hpp"

class Reader {
	public:
	Reader();

	// add more text to current context
	void addText(const std::string &text);

	// clear EVERYTHING - can be nice when switching files to get
	// rid of history that wouldn't be relevant in a backtrace for example
	void clearAll();

	// push current context and switch to new context.
	// caller must keep pointer valid until popWords()
	void pushObjList(ObjList *objs);
	// return to previous context, discarding current context
	void popObjList();
	// are there ObjLists left on the stack?
	bool hasPushedObjLists();

	// NOTE - these return Objects by value since pointers gets weird after
	// inserting/deleting objects  while iterating

	// get next object or NULLOBJ if none
	const Object nextObj();
	// peek at next object or NULLOBJ if none
	const Object peekObj();
	// get previous object or NULLOBJ if none
	const Object prevObj();
	// peek at previous object NULLOBJ if none
	const Object peekPrevObj();
	// delete the object before the current position in the stream
	void deletePrevObj();
	// insert an object before the current position (would be read by 
	// a subsequent prevObj())
	void insertPrevObj(const Object& obj);

	void debugPrintObjList() const;

	protected:
	ObjList *objlist;
	size_t pos;
	std::vector<ObjList*> stack_objlists;
	std::vector<size_t> stack_pos;
};
