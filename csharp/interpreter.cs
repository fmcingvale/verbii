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
#nullable enable

public class Interpreter {
	// definition of following are similar to C++ version, so see comments there for more detail.
	// as in C++ version, everything is public here so builtins can change anything.

	// NOTE: Many places assume that these are integers (32-bit) so don't change these to long
	// without searching for (int) casts to change as well.

	public const int STACK_SIZE = (1<<16);
	public const int HEAP_STARTSIZE = (1<<16);

	public List<LangObject> OBJMEM;
	
	// current stack pointer (points to item on top of stack), empty value and lowest usable index
	public int SP, SP_EMPTY, SP_MIN;
	// next free index to allocate in heap
	public int HEAP_NEXTFREE;

	// code currently running
	public List<LangObject>? code;
	public int codepos;
	public CallFrameData? framedata;
	// stack of previous frames (code,codepos for each)
	public List<Tuple<List<LangObject>,int,CallFrameData?>> callstack;

	// stats
	public int max_callstack;
	public int min_run_SP;
	public ulong nr_tailcalls;

	// user defined words - access with methods only
	private Dictionary<string,List<LangObject>> WORDS;
	
	public Interpreter() {
		//Console.WriteLine("*** STARTING INTERPRETER ***");
		OBJMEM = new List<LangObject>(STACK_SIZE+HEAP_STARTSIZE);
		// any better way to do this??
		for(int i=0; i<(STACK_SIZE+HEAP_STARTSIZE); ++i) {
			OBJMEM.Add(new LangInt(0));
		}

		SP_MIN = 0;
		SP_EMPTY = SP_MIN + STACK_SIZE;
		SP = SP_EMPTY;
		
		HEAP_NEXTFREE = SP_EMPTY;

		WORDS = new Dictionary<string,List<LangObject>>();

		code = null;
		codepos = -1;
		framedata = null;
		callstack = new List<Tuple<List<LangObject>,int,CallFrameData?>>();

		// stats
		max_callstack = 0;
		min_run_SP = SP;
		nr_tailcalls = 0;
	}

	public void printStats() {
		Console.WriteLine("\n==== Runtime Stats ====");

		Console.WriteLine("* General:");
		Console.WriteLine("  Builtin words: " + Builtins.builtins.Count);
		Console.WriteLine("  User-defined words: " + WORDS.Count);
		Console.WriteLine("  Max stack depth: " + (SP_EMPTY - min_run_SP));
		Console.WriteLine("  Max callstack depth: " + max_callstack);
		Console.WriteLine("  Tail calls: " + nr_tailcalls);

		Console.WriteLine("* Notices:");
		if(SP != SP_EMPTY)
			Console.WriteLine("  Stack is not empty! (" + (SP_EMPTY-SP) + " items)");		
	}

	// allocate space for nr objects, returning starting index
	public int heapAllocate(long nr) {
		int addr = HEAP_NEXTFREE;
		while((HEAP_NEXTFREE + nr) >= OBJMEM.Count) {
			// double memory when out of space
			int newsize = OBJMEM.Count * 2;
			for(int i=HEAP_NEXTFREE; i<newsize; ++i) {
				OBJMEM.Add(new LangInt(0));
			}
		}
		HEAP_NEXTFREE += (int)nr;
		return addr;
	}

	public void push(LangObject obj) {
		if(SP <= SP_MIN) {
			throw new LangError("Stack overflow");
		}
		OBJMEM[--SP] = obj;
	}

	public LangObject pop() {
		if(SP >= SP_EMPTY) {
			throw new LangError("Stack underflow");
		}
		// stats
		min_run_SP = Math.Min(min_run_SP, SP); 
		return OBJMEM[SP++];
	}

	public string reprStack() {
		string s = "";
		for(int i=SP_EMPTY-1; i>=SP; --i) {
			s += OBJMEM[i].fmtStackPrint() + " ";
		}
		return s;
	}

	public LangObject nextObj() {
		if(code == null) {
			throw new LangError("nextObj() called when not running");
		}
		if(codepos >= code.Count) {
			return new LangVoid();
		}
		return code[codepos++];
	}

	public LangObject nextObjOrFail(string where) {
		var obj = nextObj();
		if(obj is LangVoid) {
			throw new LangError("Unexpected end of input (in " + where + ")");
		}
		return obj;
	}

	public LangObject peekNextObj() {
		if(code == null) {
			throw new LangError("peekNextObj() called while not running");
		}
		if(codepos >= code.Count) {
			return new LangVoid();
		}
		return code[codepos];
	}

	public LangObject prevObj() {
		if(code == null) {
			throw new LangError("prevObj() called while not running");
		}
		if(codepos <= 0) {
			return new LangVoid();
		}
		return code[--codepos];
	}

	public LangObject prevObjOrFail(string where) {
		var obj = prevObj();
		if(obj is LangVoid) {
			throw new LangError("No previous object (in " + where + ")");
		}
		return obj;
	}

	public LangSymbol nextSymbolOrFail(string where) {
		var obj = nextObj();
		var sym = obj as LangSymbol;
		if(sym == null) {
			throw new LangError("Expecting symbol but got " + obj.fmtStackPrint() + " (in " + where + ")");
		}
		return sym;
	}

	// take symbol like '>>NAME' or '<<NAME' and jump to '@NAME'
	public void do_jump(LangSymbol jumpsym) {
		//cout << "DO_JUMP TO: " << jumpword << endl;
		if(jumpsym.match(">>",2)) {
			// forward jump, find word (>>NAME -> @NAME)
			while(true) {
				var obj = nextObj();
				if(obj is LangVoid)
					throw new LangError("No such jump: " + jumpsym.value);

				var sym = obj as LangSymbol;
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
				var obj = prevObj();
				if(obj is LangVoid)
					throw new LangError("No such jump: " + jumpsym.value);

				var sym = obj as LangSymbol;
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

	public void code_call(List<LangObject> objlist, LangBoundLambda? boundlambda=null) {
		//Console.WriteLine("CALLING");
		if(code == null) {
			throw new LangError("call while not running");
		}
		callstack.Add(Tuple.Create(code,codepos,framedata));
		code = objlist;
		codepos = 0;
		framedata = new CallFrameData();
		if(boundlambda != null)
			framedata.setOuterFrame(boundlambda.outer);

		// stats
		max_callstack = Math.Max(max_callstack,callstack.Count);
	}

	public bool havePushedFrames() {
		return callstack.Count > 0;
	}

	public void code_return() {
		//Console.WriteLine("RETURNING");
		if(!havePushedFrames()) {
			throw new LangError("return without call");
		}
		var tup = callstack[callstack.Count-1];
		code = tup.Item1;
		codepos = tup.Item2;
		framedata = tup.Item3;
		callstack.RemoveAt(callstack.Count-1);
	}

	public bool hasWord(string name) {
		return WORDS.ContainsKey(name);
	}

	public void defineWord(string name, List<LangObject> objlist, bool allow_overwrite) {
		if(hasWord(name) && !allow_overwrite)
			throw new LangError("Trying to redefine name: " + name);

		WORDS[name] = objlist;
	}

	public List<LangObject>? lookupWord(string name) {
		if(WORDS.ContainsKey(name))
			return WORDS[name];
		else
			return null;
	}

	public List<LangObject> lookupWordOrFail(string name) {
		var list = lookupWord(name);
		if(list == null)
			throw new LangError("No such word: " + name);

		return list;
	}
	
	public void deleteWord(string name) {
		if(WORDS.ContainsKey(name)) {
			WORDS.Remove(name);
		}
		else {
			throw new LangError("Trying to delete non-existent name: " + name);
		}
	}

	public LangObject getWordlist() {
		var list = new LangList();
		foreach(var pair in WORDS) {
			// no ordering requirement
			list.objlist.Add(new LangSymbol(pair.Key));
		}
		return list;
	}

	public void run(List<LangObject> objlist, Func<Interpreter,LangObject,int>? debug_hook) {
		if(callstack.Count > 0) {
			throw new LangError("Interpreter::run called recursively");
		}

		code = objlist;
		codepos = 0;
		framedata = null;
		
		//Console.WriteLine("RUNNING LIST:");
		//foreach(var obj in objlist) {
		//	Console.WriteLine("     " + obj.fmtStackPrint() + "(" + obj.typename() + ")");
		//}

		// see C++ version for comments, only brief comments here
		while(true) {
			//Console.WriteLine("TOP OF RUN LOOP");
			var obj = nextObj();
			if(debug_hook != null) {
				debug_hook(this, obj);
			}

			if (obj is LangSymbol) {
				var sym = obj as LangSymbol;	

				if(sym!.value[0] == '\'') {
					// quoted symbol - remove one level of quoting and push
					push(new LangSymbol(sym.value.Substring(1)));
					continue;
				}

				if(sym!.match("return")) {
					// return from word by popping back to previous wordlist (don't call at toplevel)
					if(havePushedFrames()) {	
						code_return();
					}
					else {
						code = null; // mark self as not running
						return; // top level return exits program
					}
					continue;
				}

				if(sym!.match("if")) {
					var cond = pop();
					var b_cond = cond as LangBool;
					if(b_cond == null)
						throw new LangError("'if' requires true|false but got: " + cond.fmtStackPrint());
					
					// cases:
					//	TRUE - run next instruction (i.e. do nothing)
					//	FALSE - skip next instruction
					if(!b_cond.value)
						codepos += 1;
			
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

				if(sym!.match("call")) {
					// top of stack must be a lambda
					var val = pop();
					// try all casts to see what i have ...
					var lambda = val as LangLambda;
					var list = val as LangList;
					var boundlambda = val as LangBoundLambda;
					if(lambda != null) {
						// now this is just like calling a userword, below
						// TODO -- tail call elimination??
						code_call(lambda.objlist);
					}
					else if(boundlambda != null) {
						code_call(boundlambda.objlist, boundlambda);
					}
					else {			
						throw new LangError("call expects a lambda or bound-lambda but got: " + val.fmtStackPrint());
					}
					continue;
				}
			
				// builtins, then userwords

				if(Builtins.builtins.ContainsKey(sym!.value)) {
					Builtins.builtins[sym!.value](this);
					continue;
				}

				if(WORDS.ContainsKey(sym!.value)) {
					// tail call elimination -- if I'm at the end of this wordlist OR next word is 'return', then
					// i don't need to come back here, so pop my wordlist first to stop stack from growing
					var next = peekNextObj();
					var nextSym = next as LangSymbol;

					if((next is LangVoid) || (nextSym != null && nextSym.match("return"))) {
						if(havePushedFrames()) { // in case i'm at the toplevel
							++nr_tailcalls;
							code_return();
						}
					}

					// execute word by pushing its wordlist and continuing
					code_call(WORDS[sym!.value]);
					continue;
				}
			}

			else if(obj is LangOpcode) {
				Opcodes.runOpcode(this, (obj as LangOpcode)!);
				continue;
			}

			else if(obj is LangVoid) {
				//Console.WriteLine("GOT VOID");
				// i could be returning from a word that had no 'return',
				// so pop words like i would if it were a return
				if(havePushedFrames()) {
					code_return();
					continue;
				}
				else {
					code = null; // mark self as not running
					return;
				}
			}

			//Console.WriteLine("STACK NOW: " + reprStack());
			//Console.WriteLine("RUN OBJ: " + obj.fmtStackPrint());

			// list literals are deepcopied (see DESIGN-NOTES.md)
			else if(obj is LangList) {
				push(obj.deepcopy());
				continue;
			}

			// everything else gets pushed here (see c++ notes for more)
			else {
				//Console.WriteLine("INTR PUSH LITERAL: " + obj.fmtStackPrint());
				push(obj);
				continue;
			}

			throw new LangError("Unknown word " + obj.fmtDisplay());
		}
	}
}
