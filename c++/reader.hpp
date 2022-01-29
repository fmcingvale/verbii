/*
	Reader - splits text into words and provides next/previous interface.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once
#include <string>
#include <vector>
#include <tuple>

class Reader {
	public:
	Reader();

	// add more text to current context
	void addText(const std::string &text);

	// push current context and switch to new context.
	// caller must keep pointer valid until popWords()
	void pushWords(std::vector<std::string> words);
	// return to previous context, discarding current context
	void popWords();

	// get next word or "" if none
	const std::string& nextWord();
	// peek at next word or "" if none
	const std::string& peekWord();
	// get previous word or "" if none
	const std::string& prevWord();

	protected:
	std::vector<std::string> words;
	size_t pos;
	std::vector<std::tuple<std::vector<std::string>,size_t>> stack;
};





