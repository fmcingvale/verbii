/*
	Exceptions
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#pragma once
#include <exception>
#include <string>

class LangError: public std::exception
{
	public:
	LangError(std::string error) { message = error; }

	virtual const char* what() const throw() {
		return message.c_str();
	}

	protected:
	std::string message;
};