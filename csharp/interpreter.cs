/*
	Interpreter - runs code.

	There is no compilation step, not really even a parsing step -- the interpreter
	runs directly from the wordlists from the Reader. This makes the code smaller and
	makes e.g. forward declarations really easy since nothing is evaluated until it
	runs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ version.
*/
using System;
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

	public Syntax syntax;

	// user defined words
	public Dictionary<string,List<LangObject>> WORDS;
	// user defined variables
	public Dictionary<string,LangMemoryArray> VARS;
	// unnamed functions
	public List<List<LangObject>> LAMBDAS;

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

		syntax = new Syntax();

		re_integer = new Regex(@"^[\+-]?\d+$");
		
		WORDS = new Dictionary<string,List<LangObject>>();
		VARS = new Dictionary<string,LangMemoryArray>();
		LAMBDAS = new List<List<LangObject>>();
	}

	public void addText(string text) {
		syntax.addText(text);
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
			s += STACKLOCALS[i].fmtStackPrint() + " ";
		}
		return s;
	}

	// take symbol like '>>NAME' or '<<NAME' and jump to '@NAME'
	public void do_jump(LangSymbol jumpsym) {
		//cout << "DO_JUMP TO: " << jumpword << endl;
		if(jumpsym.match(">>",2)) {
			// forward jump, find word (>>NAME -> @NAME)
			while(true) {
				var sym = syntax.nextObjOrFail() as LangSymbol;
				//cout << "NEXT-WORD: " << word << endl;
				if(sym != null && (sym.value.Substring(1) == jumpsym.value.Substring(2))) {
					//cout << "FOUND" << endl;
					return; // found word, stop
				}
			}
		}
		else if(jumpsym.match("<<",2)) {
			// backward jump
			while(true) {
				var sym = syntax.prevObjOrFail() as LangSymbol;
				//cout << "PREV-WORD: " << word << endl;
				if(sym != null && sym.value.Substring(1) == jumpsym.value.Substring(2)) {
					//cout << "FOUND" << endl;
					return; // found word, stop
				}
			}
		}
		else {
			throw new LangError("Bad jumpword " + jumpsym.fmtStackPrint());
		}
	}

	public void run() {
		// see C++ version for comments, only brief comments here
		while(true) {
			var obj = syntax.nextObj();
			//if(obj != null) {
			//	Console.WriteLine("RUN OBJ: " + obj.fmtStackPrint());
			//}
			if(obj == null) {
				// i could be returning from a word that had no 'return',
				// so pop words like i would if it were a return
				if(syntax.hasPushedObjLists()) {
					syntax.popObjList();
					//Console.WriteLine("POPPED OBJLIST [end], back to:");
					//syntax.debug_print_objlist();
					continue;
				}
				else {
					return;
				}
			}

			// check for immediates that get pushed
			if(obj is LangInt || obj is LangFloat || obj is LangString || obj is LangLambda) {
				//Console.WriteLine("INTR PUSH LITERAL: " + obj.fmtStackPrint());
				push(obj);
				continue;
			}

			if (obj is LangSymbol) {
				var sym = obj as LangSymbol;	

				if(sym!.match("return")) {
					// return from word by popping back to previous wordlist (don't call at toplevel)
					if(syntax.hasPushedObjLists()) {	
						syntax.popObjList();
						//Console.WriteLine("POPPED OBJLIST [return], back to:");
						//syntax.debug_print_objlist();
					}
					else {
						return; // top level return exits program
					}
					continue;
				}

				if(sym!.match("if")) {
					// true jump is required
					var true_jump = syntax.nextSymbolOrFail();
					// false word is optional
					var false_jump = syntax.peekObj() as LangSymbol;
					if(false_jump == null || false_jump.value.Length < 2 ||
						(false_jump.match("<<",2) == false && false_jump.match(">>",2) == false)) {
						false_jump = null;
					}
					var cond = pop();
					var b_cond = cond as LangBool;
					if(b_cond == null) {
						throw new LangError("'if' requires true|false but got: " + cond.fmtStackPrint());
					}
					// these don't run the jump, they just reposition the reader
					if(b_cond.value) {
						// no need to actually skip false jump since i'll be looking for '@'
						do_jump(true_jump);
					}
					else if(false_jump != null) {
						syntax.nextObj(); // only peeked above, so read it now
						do_jump(false_jump);
					}
					continue;
				}

				if(sym!.match(">>",2) || sym!.match("<<",2)) {
					do_jump(sym);
					continue;
				}

				if(sym!.match("@",1)) {
					// jump target -- ignore
					continue;
				}

				if(sym!.match("var")) {
					var name = syntax.nextSymbolOrFail();
					var count = syntax.nextObjOrFail();
					if(!(count is LangInt)) {
						throw new LangError("Count must be integer but got: " + count.fmtStackPrint());
					}
					// must be unique userword
					if(VARS.ContainsKey(name.value)) {
						throw new LangError("Trying to redefine variable " + name.value);
					}
					// add to VARS so name lookup works (below)
					VARS[name.value] = new LangMemoryArray((count as LangInt)!.value);
					continue;
				}

				if(sym!.match("del")) {
					var name = syntax.nextSymbolOrFail();
					if(!VARS.ContainsKey(name.value)) {
						throw new LangError("Trying to delete non-existent variable " + name.value);
					}
					VARS.Remove(name.value);
					continue;
				}
				
				if(sym!.match("call")) {
					// top of stack must be a lambda
					var val = pop();
					var lambda = val as LangLambda;
					if(lambda == null) {
						throw new LangError("call expects a lambda, but got: " + val.fmtStackPrint());
					}
					// now this is just like calling a userword, below
					// TODO -- tail call elimination??
					syntax.pushObjList(lambda.objlist);
					//Console.WriteLine("CALLING, new objlist:");
					//syntax.debug_print_objlist();
					continue;
				}
			
				// builtins, then userwords, then vars

				if(Builtins.builtins.ContainsKey(sym!.value)) {
					Builtins.builtins[sym!.value](this);
					continue;
				}

				if(WORDS.ContainsKey(sym!.value)) {
					// tail call elimination -- if I'm at the end of this wordlist OR next word is 'return', then
					// i don't need to come back here, so pop my wordlist first to stop stack from growing
					var next = syntax.peekObj();
					var nextSym = next as LangSymbol;

					if(next == null || (nextSym != null && nextSym.match("return"))) {
						if(syntax.hasPushedObjLists()) { // in case i'm at the toplevel
							syntax.popObjList();
						}
					}

					// execute word by pushing its wordlist and continuing
					syntax.pushObjList(WORDS[sym!.value]);
					continue;
				}

				if(VARS.ContainsKey(sym!.value)) {
					push(VARS[sym!.value]);
					continue;
				}
			}
			throw new LangError("Unknown word " + obj.fmtStackPrint());
		}
	}
}
