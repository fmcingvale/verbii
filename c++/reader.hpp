/*
	Reader - splits text into words and provides next/previous interface.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once
#include <string>
#include <vector>
#include <tuple>
#include <memory>
#include <gc/gc_cpp.h>

typedef std::vector<std::string> Wordlist;

class Reader : public gc {
	public:
	Reader();

	// add more text to current context
	void addText(const std::string &text);

	// clear EVERYTHING - can be nice when switching files to get
	// rid of history that wouldn't be relevant in a backtrace for example
	void clearAll();

	// push current context and switch to new context.
	// caller must keep pointer valid until popWords()
	void pushWords(Wordlist *words);
	// return to previous context, discarding current context
	void popWords();
	// are there wordlists left on the stack?
	bool hasPushedWords();

	// get next word or "" if none
	const std::string& nextWord();
	// peek at next word or "" if none
	const std::string& peekWord();
	// get previous word or "" if none
	const std::string& prevWord();
	// delete the word before the current position in the stream
	void deletePrevWord();
	// insert a word before the current position (would be read by 
	// a subsequent prevWord())
	void insertPrevWord(const std::string &word);

	protected:
	Wordlist *wordlist;
	size_t pos;
	std::vector<std::tuple<Wordlist*,size_t>> stack;
};
