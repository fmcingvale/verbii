/*
	Reader - splits text into words and provides next/previous interface.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	This is ported from the C++ version.
*/
using System;
using System.Collections.Generic;
#nullable enable

public class Reader {
	public Reader() {
		// dotnet build complains if I do 'clearAll()' instead of doing the same thing inline here ...
		// (doesn't see that the references are set by clearAll())
		objlist = new List<LangObject>();
		pos = 0;
		stack = new List<Tuple<List<LangObject>,int>>();
	}

	// add more text to current context
	public void addText(string text) {
		string[] whitespace = { " ", "\n", "\r", "\t" };
		string[] words = text.Split(whitespace, System.StringSplitOptions.RemoveEmptyEntries);
		foreach(var word in words) {
			objlist.Add(new LangSymbol(word));
		}
	}

	// clear EVERYTHING - can be nice when switching files to get
	// rid of history that wouldn't be relevant in a backtrace for example
	public void clearAll() {
		objlist = new List<LangObject>();
		pos = 0;
		stack = new List<Tuple<List<LangObject>,int>>();
	}

	// push current context and switch to new context.
	// caller must keep pointer valid until popObjList()
	public void pushObjList(List<LangObject> objs) {
		stack.Add(Tuple.Create(objlist,pos));
		objlist = objs;
		pos = 0;
	}

	// return to previous context, discarding current context
	public void popObjList() {
		var tup = stack[stack.Count-1];
		objlist = tup.Item1;
		pos = tup.Item2;
		stack.RemoveAt(stack.Count-1);
	}

	// are there objlists left on the stack?
	public bool hasPushedObjLists() {
		return stack.Count > 0;
	}

	// get next word or null if none
	public LangObject? nextObj() {
		if(pos >= objlist.Count) {
			return null;
		}
		else {
			return objlist[pos++];
		}
	}

	// peek at next word or null if none
	public LangObject? peekObj() {
		if(pos >= objlist.Count) {
			return null;
		}
		else {
			return objlist[pos];
		}
	}

	// get previous word or null if none
	public LangObject? prevObj() {
		if(pos <= 0) {
			return null;
		}
		else {
			return objlist[--pos];
		}
	}
	
	// peek previous word or null if none
	public LangObject? peekPrevObj() {
		if(pos <= 0) {
			return null;
		}
		else {
			return objlist[pos-1];
		}
	}

	// delete the word before the current position in the stream
	public void deletePrevObj() {
		if(pos == 0) {
			throw new LangError("No previous word to delete!");
		}
		objlist.RemoveAt(pos-1);
		--pos;
	}

	// insert a word before the current position (would be read by 
	// a subsequent prevWord())
	public void insertPrevObj(LangObject obj) {
		objlist.Insert(pos, obj);
		++pos;
	}

	public void debug_print_objlist() {
		Console.Write("WORDS: ");
		for(int i=0; i<objlist.Count; ++i) {
			if(pos == i) {
				Console.Write("[POS->] ");
			}
			Console.Write(objlist[i] + " ");
		}
		Console.Write("\n");
	}

	protected List<LangObject> objlist;
	protected int pos;
	protected List<Tuple<List<LangObject>,int>> stack;
}


