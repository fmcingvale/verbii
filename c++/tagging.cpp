/*
	Tagged values - converting native values <-> tagged uint32

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "tagging.hpp"
#include "errors.hpp"
#include <iostream>

using namespace std;

/*
	Tagged values -- 32-bit unsigned ints encoded as:

		Unsigned int: 	00 xx ..
		Signed int:   	01 xx ..
		Singletons:		10 xx ..
		Pointers:		11 xx ..

	Pointers:
		11 ttt xx ..

		ttt is the type of pointer, the remaining 27 bits is used as
		an index into a type-specific table of allocated objects.
*/

const int MAX_INT_31 = (1<<30) - 1;
const int MIN_INT_31 = -MAX_INT_31;
const unsigned int INT_31_SIGN_BIT = 1<<30;
const unsigned int MASK_30 = (1<<30) - 1;
const unsigned int BIT_32 = (1<<31);

const unsigned int BOOL_TRUE     = 0b10000000000000000000000000000001U;
const unsigned int BOOL_FALSE    = 0b10000000000000000000000000000010U;

const unsigned int MASK_TAGTYPE  = 0b11000000000000000000000000000000U;
// pointer types
const unsigned int MASK_POINTER  = 0b11000000000000000000000000000000U;
// maximum number of tagged pointers per object type
const unsigned int MAX_PTR_INDEX = 0b00000111111111111111111111111111U;
const unsigned int PTR_TYPE_MASK = 0b00111000000000000000000000000000U;
const unsigned int PTR_WORDLIST  = 0b00001000000000000000000000000000U;

tagged intToTagged(int v) {
	if(v > MAX_INT_31 || v < MIN_INT_31) {
		throw LangError("Integer overflow");
	}
	return (v < 0) ? tagged{(unsigned int)(((-v) & MASK_30) | INT_31_SIGN_BIT)} : tagged{(unsigned int)v};
}

int taggedToInt(tagged t) {
	return (t.v & INT_31_SIGN_BIT) ? -(t.v & MASK_30) : t.v;
}

bool taggedIsInt(tagged t) {
	return (t.v & BIT_32) == 0;
}

tagged boolToTagged(bool b) {
	return b ? tagged{BOOL_TRUE} : tagged{BOOL_FALSE};
}

bool taggedToBool(tagged t) {
	if(t.v == BOOL_TRUE) {
		return true;
	}
	else if(t.v == BOOL_FALSE) {
		return false;
	}
	else {
		throw LangError(string("Trying to untag bool but got: ") + to_string(t.v));
	}
}

bool taggedIsBool(tagged t) {
	return t.v == BOOL_TRUE || t.v == BOOL_FALSE;
}

// generic utilities for tagged pointers
static bool isTaggedPointer(tagged t) {
	return (t.v & MASK_TAGTYPE) == MASK_POINTER;
}

static unsigned int getTaggedPointerIndex(tagged t) {
	return (t.v & MAX_PTR_INDEX);
}

// list of all allocated Wordlists
vector<Wordlist*> tagged_wordlists;

tagged makeTaggedWordlist(Wordlist *wordlist) {
	unsigned int index = tagged_wordlists.size();
	if(index > MAX_PTR_INDEX) {
		throw LangError("Too many tagged Wordlists!");
	}
	tagged_wordlists.push_back(wordlist);
	return tagged{MASK_POINTER | PTR_WORDLIST | index};
}

bool taggedIsWordlist(tagged t) {
	return isTaggedPointer(t) && (t.v & PTR_TYPE_MASK) == PTR_WORDLIST;
}

Wordlist* taggedToWordlist(tagged t) {
	if(!taggedIsWordlist(t)) {
		throw LangError("Tagged value is not a Wordlist");
	}
	return tagged_wordlists[getTaggedPointerIndex(t)];
}

string reprTagged(tagged t) {
	if(taggedIsInt(t)) {
		return to_string(taggedToInt(t));
	}
	else if(taggedIsBool(t)) {
		return taggedToBool(t) ? "true" : "false";
	}
	else if(taggedIsWordlist(t)) {
		return "<lambda>";
	}
	else {
		throw LangError("Unknown type in reprTagged: " + to_string(t.v));
	}
}