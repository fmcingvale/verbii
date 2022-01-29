/*
	Tagged values - converting native values <-> tagged uint32
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "tagging.hpp"
#include "errors.hpp"
#include <iostream>

using namespace std;

const int MAX_INT_31 = (1<<30) - 1;
const int MIN_INT_31 = -MAX_INT_31;
const int INT_31_SIGN_BIT = 1<<30;
const int MASK_30 = (1<<30) - 1;

tagged intToTagged(int v) {
	if(v > MAX_INT_31 || v < MIN_INT_31) {
		throw LangError("Integer overflow");
	}
	return (v < 0) ? tagged{(unsigned int)(((-v) & MASK_30) | INT_31_SIGN_BIT)} : tagged{(unsigned int)v};
}

int taggedToInt(tagged v) {
	return (v.v & INT_31_SIGN_BIT) ? -(v.v & MASK_30) : v.v;
}

string reprTagged(tagged v) {
	// more later ......
	return to_string(taggedToInt(v));
}
