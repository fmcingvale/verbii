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
		Reserved:		11 xx ..
*/

const int MAX_INT_31 = (1<<30) - 1;
const int MIN_INT_31 = -MAX_INT_31;
const unsigned int INT_31_SIGN_BIT = 1<<30;
const unsigned int MASK_30 = (1<<30) - 1;
const unsigned int BIT_32 = (1<<31);

const unsigned int BOOL_TRUE  = 0b10000000000000000000000000000001U;
const unsigned int BOOL_FALSE = 0b10000000000000000000000000000010U;

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

string reprTagged(tagged t) {
	if(taggedIsInt(t)) {
		return to_string(taggedToInt(t));
	}
	else if(taggedIsBool(t)) {
		return taggedToBool(t) ? "true" : "false";
	}
	else {
		throw LangError("Unknown type in reprTagged: " + to_string(t.v));
	}
}
