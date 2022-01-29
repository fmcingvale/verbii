
/*
	Tagged values - converting native values <-> tagged uint32

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#pragma once
#include <string>

// a tagged value is just an unsigned 32-bit integer, but I want
// it to be distinct type for compiler error checking ... hopefully
// the compiler can make this as fast as using a plain int
typedef struct {
	unsigned int v;
} tagged;

tagged intToTagged(int v);
int taggedToInt(tagged v);

std::string reprTagged(tagged v);


