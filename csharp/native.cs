/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ version.
*/
using System;
using System.Collections.Generic;

class Builtins {
	public static int popInt(Interpreter intr) {
		var obj = intr.pop();
		var i = obj as LangInt;
		if(i == null) {
			throw new LangError("Expecting int but got: " + obj.repr());
		}
		return i.value;
	}

	public static void add(Interpreter intr) {
		var b = intr.pop();
		var a = intr.pop();
		var a_i = a as LangInt;
		var b_i = b as LangInt;
		if(a_i != null && b_i != null) {
			intr.push(new LangInt(a_i.value + b_i.value));
			return;
		}
		var a_m = a as LangMemoryArray;
		if(a_m != null && b_i != null) {
			var m = new LangMemoryArray(a_m);
			m.offset += b_i.value;
			intr.push(m);
			return;
		}
		
		throw new LangError("Don't know how to add " + a.repr() + " and " + b.repr());
	}

	// see C++ version for extensive comments on divmod
	public static void int_divmod(Interpreter intr) {
		var b = popInt(intr);
		var a = popInt(intr);
	
		if(b == 0) {
			throw new LangError("Divide by zero");
		}
		int quot = (int)Math.Floor(((double)(Math.Abs(a))) / ((double)(Math.Abs(b))));

		bool samesign = (a < 0 && b < 0) || (a >=0 && b >= 0);
		int mod;
		if(samesign) {
			mod = a - quot*b;
		}
		else {
			mod = a + quot*b;
			quot = -quot;
		}

		intr.push(new LangInt(mod));
		intr.push(new LangInt(quot));
	}

	public static void define_word(Interpreter intr) {
		var name = intr.nextWordOrFail();
		var words = new List<string>();
	
		while(true) {
			var w = intr.nextWordOrFail();
			if(w == ";") {
				intr.WORDS[name] = words;
				return;
			}
			else {
				words.Add(w);
			}
		}
	}

	public static void printchar(Interpreter intr) {
		int c = popInt(intr);
		char ch = (char)c;
		Console.Write(ch);
		if(c == 10 || c == 13) {
			Console.Out.Flush();
		}
	}

	// ." some string here " -- print string
	public static void print_string(Interpreter intr) {
		while(true) {
			var word = intr.nextWordOrFail();
			if(word == "\"") {
				return; // end of string
			}
			else {
				Console.Write(word + " ");
			}
		}
	}

	public static void comment(Interpreter intr) {
		while(intr.nextWordOrFail() != ")") {
		}
	}

	// set stack pointer from addr on stack
	// (SP values must be integers)
	public static void setsp(Interpreter intr) {
		int addr = popInt(intr);
		if(addr < intr.SP_MIN || addr > intr.SP_EMPTY) {
			throw new LangError("Bad address in SP!: " + addr.ToString());
		}
		intr.SP = addr;
	}

	// set locals pointer from addr on stack
	// (LP values must be integers)
	public static void setlp(Interpreter intr) {
		int addr = popInt(intr);
		if(addr < intr.LP_MIN || addr > intr.LP_EMPTY) {
			throw new LangError("Bad address in LP!: " + addr.ToString());
		}
		intr.LP = addr;
	}

	// pop top of stack and push to locals
	public static void tolocal(Interpreter intr) {
		if(intr.LP <= intr.LP_MIN) {
			throw new LangError("Locals overflow");
		}
		intr.STACKLOCALS[--intr.LP] = intr.pop();
	}

	// pop top locals and push to stack
	public static void fromlocal(Interpreter intr) {
		if(intr.LP >= intr.LP_EMPTY) {
			throw new LangError("Locals underflow");
		}
		intr.push(intr.STACKLOCALS[intr.LP++]);
	}

	// ( obj addr -- ) - save obj to addr
	//
	// two cases:
	//	* addr is integer == index into STACKLOCALS
	//	* addr is MemoryArray
	public static void set(Interpreter intr) {
		var addr = intr.pop();
		var obj = intr.pop();
		var addr_i = addr as LangInt;
		var addr_m = addr as LangMemoryArray;
		if(addr_i != null) {
			// SP or LP index
			if(addr_i.value < 0 || addr_i.value > intr.SIZE_STACKLOCALS) {
				throw new LangError("Bad address in set!: " + addr_i.value.ToString());
			}
			intr.STACKLOCALS[addr_i.value] = obj;
		}
		else if(addr_m != null) {
			if(addr_m.offset < 0 || addr_m.offset >= addr_m.array.Count()) {
				throw new LangError("Offset out of bounds in set!");
			}
			addr_m.array[addr_m.offset] = obj;
		}
		else {
			throw new LangError("NOT IMPLEMENTED IN set!");
		}
	}

	// ( addr -- obj ) load obj from addr and push to stack
	//
	// as above, addr can be int or MemoryArray
	public static void _ref(Interpreter intr) {
		var addr = intr.pop();
		var addr_i = addr as LangInt;
		var addr_m = addr as LangMemoryArray;
		if(addr_i != null) {
			if(addr_i.value < 0 || addr_i.value >= intr.SIZE_STACKLOCALS) {
				throw new LangError("Bad address in ref: " + addr_i.value.ToString());
			}
			intr.push(intr.STACKLOCALS[addr_i.value]);
		}
		else if(addr_m != null) {
			if(addr_m.offset < 0 || addr_m.offset >= addr_m.array.Count()) {
				throw new LangError("Offset out of bounds in set!");
			}
			intr.push(addr_m.array[addr_m.offset]);
		}
		else {
			throw new LangError("NOT IMPLEMENTED IN ref");
		}
	}

	// see C++ version for verbose comments; minimal comments here
	public static void make_lambda(Interpreter intr) {
		// turn { ... } into an anonymous wordlist
		
		// delete the { that was just read
		intr.reader.deletePrevWord();

		var wordlist = new List<String>();
		int nesting = 1;
		while(true) {
			var word = intr.nextWordOrFail();
			// delete the { ... } as I read it -- will replace it with a lambda reference
			intr.reader.deletePrevWord();
			if(word == "{") {
				// if I find inner lambdas, just copy them for now and later when they are run, 
				// this same process will happen for them
				++nesting;
				wordlist.Add(word);
			}
			else if(word == "}") {
				if(--nesting > 0) {
					wordlist.Add(word);
					continue;
				}
				// new unnamed wordlist will be placed into LAMBDAS, and its index placed
				// on stack and in source wordlist so a subsequent 'call' can find it
				intr.LAMBDAS.Add(wordlist);
				int index = intr.LAMBDAS.Count()-1;

				// replace { .. } in source wordlist with pseudo opcode "$$LAMBDA index" so 
				// subsequent 'call' can find it (note it would be impossible for user code 
				// to insert this word from source since it contains whitespace)
				intr.reader.insertPrevWord("$$LAMBDA " + index.ToString());
				// the first time, I have to push the lambda object as well -- interpreter
				// will do this on subsequent calls when it sees "lambda<#>"
				intr.push(new LangLambda(index));
				return;
			}
			else {
				wordlist.Add(word);
			}
		}
	}

	public static void showdef(Interpreter intr) {
		var name = intr.nextWordOrFail();
		if(!intr.WORDS.ContainsKey(name)) {
			Console.WriteLine("No such word: " + name);
			return;
		}
		var wordlist = intr.WORDS[name];
		Console.Write(name + ": ");
		foreach(var w in wordlist) {
			Console.Write(w + " ");
		}
		Console.WriteLine(";");
	}

	public static Dictionary<string,Action<Interpreter>> builtins = 
		new Dictionary<string,Action<Interpreter>> { 
		{"+", add},
		{"-", intr => intr.push(new LangInt(-popInt(intr) + popInt(intr)))},
		{"*", intr => intr.push(new LangInt(popInt(intr) * popInt(intr)))},
		{"/mod", int_divmod},
		{"==", intr => intr.push(new LangBool(popInt(intr) == popInt(intr)))},
		{">", intr => intr.push(new LangBool(popInt(intr) < popInt(intr)))},
		{":", define_word},
		{"def", define_word}, // synonym for :
		{".c", printchar},
		{".\"", print_string},
		{"repr", intr => Console.Write(intr.pop().repr())},
		{"depth", intr => intr.push(new LangInt(intr.SP_EMPTY - intr.SP))},
		{"SP", intr => intr.push(new LangInt(intr.SP))},
		{"SP!", setsp},
		{"LP", intr => intr.push(new LangInt(intr.LP))},
		{"LP!", setlp},
		{">L", tolocal},
		{"L>", fromlocal},
		{"(", comment},
		{"set!", set},
		{"ref", _ref},
		{"{", make_lambda},
		{".showdef", showdef},
		
	};
}
