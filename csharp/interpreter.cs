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

	public Reader reader;

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

		reader = new Reader();

		re_integer = new Regex(@"^[\+-]?\d+$");
		
		WORDS = new Dictionary<string,List<LangObject>>();
		VARS = new Dictionary<string,LangMemoryArray>();
		LAMBDAS = new List<List<LangObject>>();
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
			s += STACKLOCALS[i].fmtStackPrint() + " ";
		}
		return s;
	}

	public LangObject nextObjOrFail() {
		if(reader.peekObj() == null) {
			throw new LangError("Unexpected end of input");
		}
		return reader.nextObj()!;
	}

	public LangSymbol nextSymbolOrFail() {
		var obj = nextObjOrFail();
		if(!(obj is LangSymbol)) {
			throw new LangError("Expecting symbol but got " + obj.fmtStackPrint());
		}
		return (obj as LangSymbol)!;
	}

	public LangObject prevObjOrFail() {
		if(reader.peekPrevObj() == null) {
			throw new LangError("Unable to find previous word");
		}
		return reader.prevObj()!;
	}

	// take symbol like '>>NAME' or '<<NAME' and jump to '@NAME'
	public void do_jump(LangSymbol jumpsym) {
		//cout << "DO_JUMP TO: " << jumpword << endl;
		if(jumpsym.match(">>",2)) {
			// forward jump, find word (>>NAME -> @NAME)
			while(true) {
				var sym = nextObjOrFail() as LangSymbol;
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
				var sym = prevObjOrFail() as LangSymbol;
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
			var obj = reader.nextObj();
			if(obj == null) {
				// i could be returning from a word that had no 'return',
				// so pop words like i would if it were a return
				if(reader.hasPushedObjLists()) {
					reader.popObjList();
					continue;
				}
				else {
					return;
				}
			}

			if (obj is LangSymbol) {
				var sym = obj as LangSymbol;	

				// push integers to stack
				if(re_integer.IsMatch(sym!.value)) {
					int i = int.Parse(sym!.value);
					push(new LangInt(i));
					continue;
				}

				// floats: #NNN.NN
				if(sym!.match("#",1)) {
					double d = double.Parse(sym!.value.Substring(1));
					push(new LangFloat(d));
					continue;
				}

				// check for "$$LAMBDA index"
				// TODO -- remove me and push lambda to wordlist directly
				if(sym!.value.Length >= 10 && sym!.value.Substring(0,9) == "$$LAMBDA ") {
					var index = int.Parse(sym!.value.Substring(9));
					push(new LangLambda(index));
					continue;
				}

				if(sym!.match("return")) {
					// return from word by popping back to previous wordlist (don't call at toplevel)
					if(reader.hasPushedObjLists()) {	
						reader.popObjList();
					}
					else {
						return; // top level return exits program
					}
					continue;
				}

				if(sym!.match("if")) {
					// true jump is required
					var true_jump = nextSymbolOrFail();
					// false word is optional
					var false_jump = reader.peekObj() as LangSymbol;
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
						reader.nextObj(); // only peeked above, so read it now
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
					var name = nextSymbolOrFail();
					var count = int.Parse(nextSymbolOrFail().value);
					// must be unique userword
					if(VARS.ContainsKey(name.value)) {
						throw new LangError("Trying to redefine variable " + name.value);
					}
					// add to VARS so name lookup works (below)
					VARS[name.value] = new LangMemoryArray(count);
					continue;
				}

				if(sym!.match("del")) {
					var name = nextSymbolOrFail();
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
					reader.pushObjList(LAMBDAS[lambda.index]);
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
					var nextObj = reader.peekObj();
					var nextSym = reader.peekObj() as LangSymbol;

					if(nextObj == null || nextSym == null || nextSym.match("return")) {
						if(reader.hasPushedObjLists()) { // in case i'm at the toplevel
							reader.popObjList();
						}
					}
					// execute word by pushing its wordlist and continuing
					reader.pushObjList(WORDS[sym!.value]);
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
