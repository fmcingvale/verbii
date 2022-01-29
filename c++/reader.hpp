/*
	Reader - splits text into words and provides next/previous interface.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once
#include <string>
#include <vector>
#include <tuple>
#include <memory>

typedef std::vector<std::string> Wordlist;

class Reader {
	public:
	Reader();

	// add more text to current context
	void addText(const std::string &text);

	// push current context and switch to new context.
	// caller must keep pointer valid until popWords()
	void pushWords(Wordlist *words);
	// return to previous context, discarding current context
	void popWords();
	// are their wordlists left on the stack?
	bool hasPushedWords();

	// get next word or "" if none
	const std::string& nextWord();
	// peek at next word or "" if none
	const std::string& peekWord();
	// get previous word or "" if none
	const std::string& prevWord();

	protected:
	Wordlist *wordlist;
	size_t pos;
	std::vector<std::tuple<Wordlist*,size_t>> stack;
};





