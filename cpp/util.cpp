/*
	Utility functions used in a few places.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "util.hpp"
#include "errors.hpp"
#include <sys/stat.h>
using namespace std;

#ifdef _MSC_VER
#include <io.h>
bool file_exists(const string& filename) {
	return _access_s(filename.c_str(), 0) == 0;
}
#else // assume posix
bool file_exists(const string &filename) {
	struct stat st;
	if(stat(filename.c_str(),&st)<0)
		return false;

	return S_ISREG(st.st_mode) != 0;
}
#endif

#ifdef _MSC_VER
int file_size(const string& filename) {
	struct _stat st;
	if (_stat(filename.c_str(), &st) != 0)
		throw LangError("Trying to get size of nonexistent file: " + filename);

	return st.st_size;
}
#else // assume posix
int file_size(const string &filename) {
	struct stat st;
	if(stat(filename.c_str(),&st)<0)
		throw LangError("Trying to get size of nonexistent file: " + filename);

	return (int)st.st_size;
}
#endif
