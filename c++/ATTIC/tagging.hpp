
/*
	Tagged values - converting native values <-> tagged uint32

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#pragma once

typedef unsigned int uint32;

uint32 intToTagged(int v);
int taggedToInt(uint32 v);

