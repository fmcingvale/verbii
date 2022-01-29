/*
	Reader unittests

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
	reader.addText("  1-aaa  1-bbb 1-ccc  1-ddd  ");

	CHECK(reader.nextWord() == "1-aaa");
	CHECK(reader.nextWord() == "1-bbb");

	Wordlist more{"2-aaa","2-bbb","2-ccc"};
	reader.pushWords(&more);
	
	CHECK(reader.nextWord() == "2-aaa");
	CHECK(reader.nextWord() == "2-bbb");
	CHECK(reader.nextWord() == "2-ccc");
	
	Wordlist more2{"3-aaa","3-bbb","3-ccc"};
	reader.pushWords(&more2);
	
	CHECK(reader.nextWord() == "3-aaa");
	CHECK(reader.nextWord() == "3-bbb");
	CHECK(reader.nextWord() == "3-ccc");
	CHECK(reader.nextWord() == "");
	
	CHECK(reader.prevWord() == "3-ccc");
	reader.popWords();
	
	CHECK(reader.prevWord() == "2-ccc");
	CHECK(reader.prevWord() == "2-bbb");
	
	reader.popWords();

	CHECK(reader.nextWord() == "1-ccc");
	CHECK(reader.nextWord() == "1-ddd");
}



	
