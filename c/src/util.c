/*
	Utility functions used in a few places.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "util.h"
#include "errors.h"
#include "langtypes.h"
#include <sys/stat.h>
#include <time.h>

#ifdef _MSC_VER
#include <io.h>
int file_exists(const char* filename) {
	return (_access_s(filename, 0) == 0) ? TRUE : FALSE;
}
#else // assume posix
int file_exists(const char *filename) {
	struct stat st;
	if(stat(filename,&st)<0)
		return FALSE;

	return S_ISREG(st.st_mode) != 0;
}
#endif

#ifdef _MSC_VER
int file_size(const char *filename) {
	struct _stat st;
	if (_stat(filename, &st) != 0)
		error("Trying to get size of nonexistent file: %s", filename);

	return st.st_size;
}
#else // assume posix
int file_size(const char *filename) {
	struct stat st;
	if(stat(filename,&st)<0)
		error("Trying to get size of nonexistent file: %s", filename);

	return (int)st.st_size;
}
#endif

double current_system_cpu_time() {
	return ((double)clock())/CLOCKS_PER_SEC;
}
