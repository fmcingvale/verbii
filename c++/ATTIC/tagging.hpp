
/*
	Tagged values - converting native values <-> tagged uint32

	Tagged values are a lot more efficient than making everything an object.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#pragma once
#include <string>
#include "reader.hpp"

// a tagged value is just an unsigned 32-bit integer, but I want
// it to be a distinct type for compiler error checking ... hopefully
// the compiler can make this as fast as using a plain int
typedef struct {
	unsigned int v;
} tagged;

tagged intToTagged(int v);
int taggedToInt(tagged t);
bool taggedIsInt(tagged t);

tagged boolToTagged(bool b);
bool taggedToBool(tagged t);
bool taggedIsBool(tagged t);

tagged makeTaggedWordlist(Wordlist *wordlist);
Wordlist* taggedToWordlist(tagged t);
bool taggedIsWordlist(tagged t);

std::string reprTagged(tagged t);


