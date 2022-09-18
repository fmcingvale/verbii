/*
	Utility functions used in a few places.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once
#include <string>

bool file_exists(const std::string &filename);
int file_size(const std::string &filename);

double current_system_cpu_time();