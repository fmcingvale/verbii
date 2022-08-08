/*
	Utility functions used in a few places.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "util.hpp"
#include "errors.hpp"
#include <sys/stat.h>
using namespace std;

bool file_exists(const string &filename) {
	struct stat st;
	if(stat(filename.c_str(),&st)<0)
		return false;

	return S_ISREG(st.st_mode) != 0;
}

int file_size(const string &filename) {
	struct stat st;
	if(stat(filename.c_str(),&st)<0)
		throw LangError("Trying to get size of nonexistent file: " + filename);

	return (int)st.st_size;
}
