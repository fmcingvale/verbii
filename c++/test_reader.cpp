/*
	Reader unittests

	I do NOT want to have unittests for everything, since I think running code in
	the interpreter is the best test. However, it's hard to test some low level details
	of the Reader just with code, so I made an exception here.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "extern/doctest.h"
#include "reader.hpp"
#include <string>
#include <vector>
#include <iostream>
using namespace std;

TEST_CASE("reader:forward-1") {
	auto reader = Reader();
	reader.addText("one two three four");

	CHECK(reader.nextWord() == "one");
	CHECK(reader.nextWord() == "two");
	CHECK(reader.nextWord() == "three");
	CHECK(reader.nextWord() == "four");
	CHECK(reader.nextWord() == "");
}

TEST_CASE("reader:forward-2") {
	auto reader = Reader();
	reader.addText("   def :\nhello .\"   1234   world !   ");
	CHECK(reader.nextWord() == "def");
	CHECK(reader.nextWord() == ":");
	CHECK(reader.nextWord() == "hello");
	CHECK(reader.nextWord() == ".\"");
	CHECK(reader.nextWord() == "1234");
	CHECK(reader.nextWord() == "world");
	CHECK(reader.nextWord() == "!");
	CHECK(reader.nextWord() == "");
}

TEST_CASE("reader:forward-backward") {
	auto reader = Reader();
	reader.addText("  one 123 \r\n  three 456  \t \n\r five    ");

	CHECK(reader.nextWord() == "one");
	CHECK(reader.nextWord() == "123");
	CHECK(reader.nextWord() == "three");

	CHECK(reader.prevWord() == "three");
	CHECK(reader.prevWord() == "123");
	
	CHECK(reader.nextWord() == "123");
	CHECK(reader.nextWord() == "three");

	CHECK(reader.nextWord() == "456");
	CHECK(reader.nextWord() == "five");
	CHECK(reader.nextWord() == "");
	
	CHECK(reader.prevWord() == "five");
	CHECK(reader.prevWord() == "456");
	CHECK(reader.prevWord() == "three");
	CHECK(reader.prevWord() == "123");
	CHECK(reader.prevWord() == "one");
	CHECK(reader.prevWord() == "");
}

TEST_CASE("reader:push-pop") {
	auto reader = Reader();
	
	CHECK(reader.hasPushedWords() == false);
	reader.addText("  1-aaa  1-bbb 1-ccc  1-ddd  ");
	CHECK(reader.hasPushedWords() == false);

	CHECK(reader.nextWord() == "1-aaa");
	CHECK(reader.peekWord() == "1-bbb");
	CHECK(reader.nextWord() == "1-bbb");

	Wordlist more{"2-aaa","2-bbb","2-ccc"};
	CHECK(reader.hasPushedWords() == false);
	reader.pushWords(&more);
	CHECK(reader.hasPushedWords() == true);
	
	CHECK(reader.nextWord() == "2-aaa");
	CHECK(reader.nextWord() == "2-bbb");
	CHECK(reader.peekWord() == "2-ccc");
	CHECK(reader.nextWord() == "2-ccc");
	
	Wordlist more2{"3-aaa","3-bbb","3-ccc"};
	CHECK(reader.hasPushedWords() == true);
	reader.pushWords(&more2);
	CHECK(reader.hasPushedWords() == true);
	
	CHECK(reader.nextWord() == "3-aaa");
	CHECK(reader.peekWord() == "3-bbb");
	CHECK(reader.nextWord() == "3-bbb");
	CHECK(reader.nextWord() == "3-ccc");
	CHECK(reader.peekWord() == "");
	CHECK(reader.nextWord() == "");
	
	CHECK(reader.prevWord() == "3-ccc");
	CHECK(reader.hasPushedWords() == true);
	reader.popWords();
	CHECK(reader.hasPushedWords() == true);
	
	CHECK(reader.prevWord() == "2-ccc");
	CHECK(reader.peekWord() == "2-ccc");
	CHECK(reader.prevWord() == "2-bbb");
	
	CHECK(reader.hasPushedWords() == true);
	reader.popWords();
	CHECK(reader.hasPushedWords() == false);
	
	CHECK(reader.nextWord() == "1-ccc");
	CHECK(reader.peekWord() == "1-ddd");
	CHECK(reader.nextWord() == "1-ddd");
}

TEST_CASE("reader:insert+delete") {
	auto reader = Reader();
	reader.addText("   one  two   three  four   ");
	CHECK(reader.nextWord() == "one");
	CHECK(reader.nextWord() == "two");
	reader.deletePrevWord();
	CHECK(reader.nextWord() == "three");
	CHECK(reader.prevWord() == "three");
	CHECK(reader.prevWord() == "one");
	CHECK(reader.prevWord() == "");

	CHECK(reader.nextWord() == "one");
	CHECK(reader.nextWord() == "three");
	reader.insertPrevWord("new-1");
	CHECK(reader.nextWord() == "four");
	CHECK(reader.prevWord() == "four");
	CHECK(reader.prevWord() == "new-1");
	CHECK(reader.prevWord() == "three");
	CHECK(reader.prevWord() == "one");
	reader.insertPrevWord("new-2");
	CHECK(reader.prevWord() == "new-2");
	CHECK(reader.nextWord() == "new-2");
	CHECK(reader.nextWord() == "one");
	
	reader.clearAll();
	reader.addText("  111   222   333   ");
	CHECK(reader.nextWord() == "111");
	reader.deletePrevWord();
	reader.insertPrevWord("new-1");
	reader.insertPrevWord("new-2");
	CHECK(reader.prevWord() == "new-2");
	CHECK(reader.prevWord() == "new-1");
	CHECK(reader.prevWord() == "");
	CHECK(reader.nextWord() == "new-1");
	CHECK(reader.nextWord() == "new-2");
	CHECK(reader.nextWord() == "222");
	CHECK(reader.nextWord() == "333");
	CHECK(reader.nextWord() == "");
}


	
