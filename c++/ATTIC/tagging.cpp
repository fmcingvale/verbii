
#include "tagging.hpp"
#include <iostream>

using namespace std;

const int MAX_INT_31 = (1<<30) - 1;
const int MIN_INT_31 = -MAX_INT_31;
const int INT_31_SIGN_BIT = 1<<30;
const int MASK_30 = (1<<30) - 1;

uint32 intToTagged(int v) {
	if(v > MAX_INT_31 || v < MIN_INT_31) {
		cout << "*** INTEGER OVER/UNDEFLOW ***\n";
		return MAX_INT_31; // hopefully makes it obvious that something is wrong
	}
	return (v < 0) ? (((-v) & MASK_30) | INT_31_SIGN_BIT) : v;
}

int taggedToInt(uint32 v) {
	return (v & INT_31_SIGN_BIT) ? -(v & MASK_30) : v;
}