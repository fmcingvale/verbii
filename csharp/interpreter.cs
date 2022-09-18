/*
	Interpreter - runs code.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ version.
*/
using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
#nullable enable

// callstack is of fixed size with these entries
public class CallstackEntry {
	public List<LangObject>? code;
	public int pos;
	public CallFrameData framedata;

	public CallstackEntry() {
		code = null;
		pos = 0;
		framedata = new CallFrameData();
	}
}

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

	// sync with C++ to ensure code runs the same on all ports
	public const int MAX_CALLSTACK_DEPTH = 1024;

	// current & previous frames
	public CallstackEntry[] callstack;
	public int callstack_cur; // -1 if no code running

	// shortcuts into callstack for current frame
	public List<LangObject>? code;
	public int codepos;
	public CallFrameData framedata;
	
	// stats
	public int max_callstack;
	public int min_run_SP;
	public ulong nr_tailcalls;
	public int nr_saved_frames;

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

		callstack = new CallstackEntry[MAX_CALLSTACK_DEPTH];
		for(int i=0; i<MAX_CALLSTACK_DEPTH; ++i) {
			callstack[i] = new CallstackEntry();
		}

		callstack_cur = -1;

		WORDS = new Dictionary<string,List<LangObject>>();

		code = null;
		codepos = -1;
		// will be set correctly later, for now just need to set to something non-null
		framedata = callstack[0].framedata;
		
		// stats
		max_callstack = 0;
		min_run_SP = SP;
		nr_tailcalls = 0;
		nr_saved_frames = 0;
	}

	public void printStats() {
		Console.WriteLine("\n==== Runtime Stats ====");

		Console.WriteLine("* General:");
		Console.WriteLine("  Builtin words: " + Builtins.builtins.Count);
		Console.WriteLine("  User-defined words: " + WORDS.Count);
		Console.WriteLine("  Max stack depth: " + (SP_EMPTY - min_run_SP));
		Console.WriteLine("  Max callstack depth: " + max_callstack);
		Console.WriteLine("  Saved frames: " + nr_saved_frames);
		Console.WriteLine("  Tail calls: " + nr_tailcalls);
		Console.WriteLine("  Total time: " + (Builtins.current_system_tick_time() - Builtins.STARTUP_TIME));
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
		if(callstack_cur < 0)
			throw new LangError("call while not running");
		
		else if(callstack_cur >= (MAX_CALLSTACK_DEPTH-1))
			throw new LangError("Max callstack depth exceeded");

		// save current context by updating .pos - no other fields need to be updated
		callstack[callstack_cur].pos = codepos;

		// setup new frame
		++callstack_cur;

		callstack[callstack_cur].code = objlist;
		callstack[callstack_cur].pos = 0;
		// currently framedata not cleared

		// set shortcuts used elsewhere
		code = objlist;
		codepos = 0;
		framedata = callstack[callstack_cur].framedata;

		if(boundlambda != null)
			framedata.setOuterFrame(boundlambda.outer);

		// stats
		max_callstack = Math.Max(max_callstack,callstack_cur+1);
	}

	public bool havePushedFrames() {
		return callstack_cur >= 0;
	}

	public void code_return() {
		//Console.WriteLine("RETURNING");
		if(callstack_cur < 0)
			throw new LangError("return without call");
		
		// is framedata now bound?
		if(framedata.isBound()) {
			// replace with fresh frame
			callstack[callstack_cur].framedata = new CallFrameData();
			++nr_saved_frames;
		}

		// pop frame
		--callstack_cur;
		// set shortcuts, unless i just popped the last frame (in which case these
		// won't be used again)
		if(callstack_cur >= 0) {
			code = callstack[callstack_cur].code;
			codepos = callstack[callstack_cur].pos;
			framedata = callstack[callstack_cur].framedata;
		}
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
		if(callstack_cur >= 0) {
			throw new LangError("Interpreter::run called recursively");
		}

		// almost but not quite code_call() so have to do it here
		callstack_cur = 0;
		callstack[callstack_cur].code = objlist;
		callstack[callstack_cur].pos = 0;
		// framedata not currently zeroed/initialized on calls

		// set shortcuts that are used everywhere else
		code = objlist;
		codepos = 0;
		framedata = callstack[callstack_cur].framedata;
		
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
					// pop frame
					code_return();
					// did i pop the last frame?
					if(callstack_cur < 0)
						return; // top level return exits program
					
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

					// FIXME - currently not done at top level since code_call expects a non-empty stack
					if(callstack_cur >= 1 && ((next is LangVoid) || (nextSym != null && nextSym.match("return")))) {
						++nr_tailcalls;
						code_return();
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
				// so handle like return above
				code_return();
				if(callstack_cur < 0)
					return; // returning from top frame

				continue;
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
