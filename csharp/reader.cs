/*
	Reader - splits text into words and provides next/previous interface.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	This is ported from the C++ version.
*/
using System;
using System.Collections.Generic;

public class Reader {
	public Reader() {
		// dotnet build complains if I do 'clearAll()' instead of doing the same thing inline here ...
		// (doesn't see that the references are set by clearAll())
		wordlist = new List<string>();
		pos = 0;
		stack = new List<Tuple<List<string>,int>>();
	}

	// add more text to current context
	public void addText(string text) {
		string[] whitespace = { " ", "\n", "\r", "\t" };
		string[] words = text.Split(whitespace, System.StringSplitOptions.RemoveEmptyEntries);
		foreach(var word in words) {
			wordlist.Add(word);
		}
	}

	// clear EVERYTHING - can be nice when switching files to get
	// rid of history that wouldn't be relevant in a backtrace for example
	public void clearAll() {
		wordlist = new List<string>();
		pos = 0;
		stack = new List<Tuple<List<string>,int>>();
	}

	// push current context and switch to new context.
	// caller must keep pointer valid until popWords()
	public void pushWords(List<string> words) {
		stack.Add(Tuple.Create(wordlist,pos));
		wordlist = new List<string>();
		pos = 0;
		foreach(var word in words) {
			wordlist.Add(word);
		}
	}

	// return to previous context, discarding current context
	public void popWords() {
		var tup = stack[stack.Count-1];
		wordlist = tup.Item1;
		pos = tup.Item2;
		stack.RemoveAt(stack.Count-1);
	}

	// are there wordlists left on the stack?
	public bool hasPushedWords() {
		return stack.Count > 0;
	}

	// get next word or "" if none
	public string nextWord() {
		if(pos >= wordlist.Count) {
			return "";
		}
		else {
			return wordlist[pos++];
		}
	}

	// peek at next word or "" if none
	public string peekWord() {
		if(pos >= wordlist.Count) {
			return "";
		}
		else {
			return wordlist[pos];
		}
	}

	// get previous word or "" if none
	public string prevWord() {
		if(pos <= 0) {
			return "";
		}
		else {
			return wordlist[--pos];
		}
	}
	
	// peek previous word or "" if none
	public string peekPrevWord() {
		if(pos <= 0) {
			return "";
		}
		else {
			return wordlist[pos-1];
		}
	}

	// delete the word before the current position in the stream
	public void deletePrevWord() {
		if(pos == 0) {
			throw new LangError("No previous word to delete!");
		}
		wordlist.RemoveAt(pos-1);
		--pos;
	}

	// insert a word before the current position (would be read by 
	// a subsequent prevWord())
	public void insertPrevWord(string word) {
		wordlist.Insert(pos, word);
		++pos;
	}

	protected List<string> wordlist;
	protected int pos;
	protected List<Tuple<List<string>,int>> stack;
}


