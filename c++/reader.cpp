
#include "reader.hpp"
#include <iostream>
using namespace std;

Reader::Reader() {
	//words = new vector<string>();
	pos = 0;
}

// add more text to current context
void Reader::addText(const string &text) {
	size_t i=0;
	while(i < text.length()) {
		while(i < text.length() && isspace(text[i])) {
			++i;
		}

		string word;
		while(i < text.length() && !isspace(text[i])) {
			word.push_back(text[i++]);
		}

		if(word.size() > 0) {
			words.push_back(word);
		}
	}	
}


// push current context and switch to new context
void Reader::pushWords(std::vector<std::string> new_words) {
	auto t = make_tuple(words,pos);

	stack.push_back(make_tuple(words,pos));

	words = new_words;
	pos = 0;
}

void Reader::popWords() {
	auto t = stack.back();
	stack.pop_back();

	words = get<0>(t);
	pos = get<1>(t);
}

static string NONE("");

const string& Reader::nextWord() {
	if(pos >= words.size()) {
		return NONE;
	}
	else {
		return words[pos++];
	}
}

const string& Reader::peekWord() {
	if(pos >= words.size()) {
		return NONE;
	}
	else {
		return words[pos];
	}
}

const string& Reader::prevWord() {
	if(pos <= 0) {
		return NONE;
	}
	else {
		return words[--pos];
	}
}
