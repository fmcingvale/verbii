/*
	Interpreter - runs code.

	There is no compilation step, not really even a parsing step -- the interpreter
	runs directly from the wordlists from the Reader. This makes the code smaller and
	makes e.g. forward declarations really easy since nothing is evaluated until it
	runs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ version.
*/

using System.Collections.Generic;
using System.Text.RegularExpressions;

public class Interpreter {
	// definition of following are similar to C++ version, so see comments there for more detail.
	// as in C++ version, everything is public here so builtins can change anything.
	public const int STACK_SIZE = (1<<10);
	public const int LOCALS_SIZE = (1<<10);

	public List<LangObject> STACKLOCALS;
	public int SIZE_STACKLOCALS;
	
	// current stack pointer (points to item on top of stack), empty value and lowest usable index
	public int SP, SP_EMPTY, SP_MIN;
	// same for locals
	public int LP, LP_EMPTY, LP_MIN;

	protected Regex re_integer;

	Reader reader;

	// user defined words
	public Dictionary<string,List<string>> WORDS;

	public Interpreter() {
		SIZE_STACKLOCALS = STACK_SIZE + LOCALS_SIZE;
		STACKLOCALS = new List<LangObject>(SIZE_STACKLOCALS);
		// any better way to do this??
		for(int i=0; i<STACK_SIZE+LOCALS_SIZE; ++i) {
			STACKLOCALS.Add(new LangObject());
		}

		SP_EMPTY = SIZE_STACKLOCALS;
		SP = SP_EMPTY;
		SP_MIN = SP_EMPTY - STACK_SIZE;

		LP_EMPTY = SP_MIN;
		LP = LP_EMPTY;
		LP_MIN = LP_EMPTY - LOCALS_SIZE;
		// sanity that I did that math correctly ...
		if(LP_MIN != 0) {
			throw new LangError("stacklocals size is wrong!");
		}

		reader = new Reader();

		re_integer = new Regex(@"^[\+-]?\d+$");
		
		WORDS = new Dictionary<string,List<string>>();
	}

	public void addText(string text) {
		reader.addText(text);
	}

	public void push(LangObject obj) {
		if(SP <= SP_MIN) {
			throw new LangError("Stack overflow");
		}
		STACKLOCALS[--SP] = obj;
	}

	public LangObject pop() {
		if(SP >= SP_EMPTY) {
			throw new LangError("Stack underflow");
		}
		return STACKLOCALS[SP++];
	}

	public string reprStack() {
		string s = "";
		for(int i=SP_EMPTY-1; i>=SP; --i) {
			s += STACKLOCALS[i].repr() + " ";
		}
		return s;
	}

	public string nextWordOrFail() {
		if(reader.peekWord() == "") {
			throw new LangError("Unexpected end of input");
		}
		return reader.nextWord();
	}

	public string prevWordOrFail() {
		if(reader.peekPrevWord() == "") {
			throw new LangError("Unable to find previous word");
		}
		return reader.prevWord();
	}

	public void run() {
		// see C++ version for comments, only brief comments here
		while(true) {
			var word = reader.nextWord();
			if(word == "") {
				// i could be returning from a word that had no 'return',
				// so pop words like i would if it were a return
				if(reader.hasPushedWords()) {
					reader.popWords();
					continue;
				}
				else {
					return;
				}
			}

			// push integers to stack
			if(re_integer.IsMatch(word)) {
				int i = int.Parse(word);
				push(new LangInt(i));
				continue;
			}

			if(Builtins.builtins.ContainsKey(word)) {
				Builtins.builtins[word](this);
				continue;
			}

			if(WORDS.ContainsKey(word)) {
				// tail call elimination -- if I'm at the end of this wordlist OR next word is 'return', then
				// i don't need to come back here, so pop my wordlist first to stop stack from growing
				if(reader.peekWord() == "" || reader.peekWord() == "return") {
					if(reader.hasPushedWords()) { // in case i'm at the toplevel
						reader.popWords();
					}
				}
				// execute word by pushing its wordlist and continuing
				reader.pushWords(WORDS[word]);
				continue;
			}

			throw new LangError("Unknown word " + word);
		}
	}
}
