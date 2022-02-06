/*
	Reader - splits text into words and provides next/previous interface.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "xmalloc.hpp"
#include "reader.hpp"
#include "errors.hpp"
#include <iostream>
#include <sstream>

using namespace std;

Reader::Reader() {
	wordlist = new Wordlist();
	pos = 0;
}

// add more text to current context
void Reader::addText(const string &text) {
	istringstream input(text);
	string word;
	while(true) {
		input >> word; // read words, delimited by whitespace
		if(input.fail()) {
			return; // reached end of file (probably some whitespace was left, so failed to read a word)
		}
		wordlist->push_back(word);
	}
}

void Reader::clearAll() {
	stack_wordlists.clear();
	stack_pos.clear();
	delete wordlist;
	wordlist = new Wordlist();
	pos = 0;
}

// push current context and switch to new context
void Reader::pushWords(Wordlist *new_words) {
	stack_wordlists.push_back(wordlist);
	stack_pos.push_back(pos);
	
	wordlist = new_words;
	pos = 0;
}

void Reader::popWords() {
	wordlist = stack_wordlists.back();
	pos = stack_pos.back();
	stack_wordlists.pop_back();
	stack_pos.pop_back();
}

bool Reader::hasPushedWords() {
	return stack_wordlists.size() > 0;
}

static string NONE("");

const string& Reader::nextWord() {
	if(pos >= wordlist->size()) {
		return NONE;
	}
	else {
		return wordlist->at(pos++);
	}
}

const string& Reader::peekWord() {
	if(pos >= wordlist->size()) {
		return NONE;
	}
	else {
		return wordlist->at(pos);
	}
}

const string& Reader::prevWord() {
	if(pos <= 0) {
		return NONE;
	}
	else {
		return wordlist->at(--pos);
	}
}

const string& Reader::peekPrevWord() {
	if(pos <= 0) {
		return NONE;
	}
	else {
		return wordlist->at(pos-1);
	}
}

void Reader::deletePrevWord() {
	if(pos == 0) {
		throw LangError("No previous word to delete!");
	}
	wordlist->erase(wordlist->begin()+pos-1);
	--pos;
}

void Reader::insertPrevWord(const string &word) {
	wordlist->insert(wordlist->begin()+pos, word);
	++pos;
}

