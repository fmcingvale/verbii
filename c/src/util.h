/*
	Utility functions used in a few places.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#ifndef __util_h__
#define __util_h__

int file_exists(const char *filename);
int file_size(const char *filename);

double current_system_cpu_time();

#endif // __util_h__
