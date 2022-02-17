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
			throw new LangError("Expecting int but got: " + obj.fmtStackPrint());
		}
		return i.value;
	}

	public static double popFloatOrInt(Interpreter intr) {
		var obj = intr.pop();
		if (obj is LangInt) {
			return (double)((obj as LangInt)!.value);
		}
		else if (obj is LangFloat) {
			return (obj as LangFloat)!.value;
		}
		else {
			throw new LangError("Expecting int or float but got: " + obj.fmtStackPrint());
		}
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
		// swapped args from above
		var b_m = b as LangMemoryArray;
		if(b_m != null && a_i != null) {
			var m = new LangMemoryArray(b_m);
			m.offset += a_i.value;
			intr.push(m);
			return;
		}
		
		throw new LangError("Don't know how to add " + a.fmtStackPrint() + " and " + b.fmtStackPrint());
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
		var name = intr.syntax.nextSymbolOrFail();
		var objlist = new List<LangObject>();
	
		while(true) {
			var o = intr.syntax.nextObjOrFail();
			var sym = o as LangSymbol;
			if(sym != null && sym.match(";")) {
				intr.WORDS[name.value] = objlist;
				return;
			}
			else {
				objlist.Add(o);
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
			if(addr_m.offset < 0 || addr_m.offset >= addr_m.array.Count) {
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
			if(addr_m.offset < 0 || addr_m.offset >= addr_m.array.Count) {
				throw new LangError("Offset out of bounds in ref");
			}
			intr.push(addr_m.array[addr_m.offset]);
		}
		else {
			throw new LangError("NOT IMPLEMENTED IN ref");
		}
	}

	public static void showdef(Interpreter intr) {
		var name = intr.syntax.nextSymbolOrFail();
		if(!intr.WORDS.ContainsKey(name.value)) {
			Console.WriteLine("No such word: " + name);
			return;
		}
		var objlist = intr.WORDS[name.value];
		Console.Write(name.value + ": ");
		foreach(var o in objlist) {
			Console.Write(o.fmtStackPrint() + " ");
		}
		Console.WriteLine(";");
	}

	public static void fdiv(Interpreter intr) {
		var b = popFloatOrInt(intr);
		var a = popFloatOrInt(intr);
		if(b == 0) {
			throw new LangError("Floating point divide by zero");
		}
		intr.push(new LangFloat(a/b));
	}

	public static void _puts(Interpreter intr) {
		//Console.WriteLine("_PUTS");
		var o = intr.pop();
		var s = o as LangString;
		if(s == null) {
			throw new LangError("puts requires string but got: " + o.fmtStackPrint());
		}
		else {
			Console.Write(s.value);
		}
	}

	public static Dictionary<string,Action<Interpreter>> builtins = 
		new Dictionary<string,Action<Interpreter>> { 
		{"+", add},
		{"-", intr => intr.push(new LangInt(-popInt(intr) + popInt(intr)))},
		{"*", intr => intr.push(new LangInt(popInt(intr) * popInt(intr)))},
		{"f+", intr  => intr.push(new LangFloat(popFloatOrInt(intr) + popFloatOrInt(intr)))},
		{"f-", intr => intr.push(new LangFloat(-popFloatOrInt(intr) + popFloatOrInt(intr)))},
		{"f*", intr  => intr.push(new LangFloat(popFloatOrInt(intr) * popFloatOrInt(intr)))},
		{"f/", fdiv},
		{"f.setprec", intr => LangFloat.FLOAT_PRECISION = popInt(intr)},
		{"/mod", int_divmod},
		{"==", intr => intr.push(new LangBool(popFloatOrInt(intr) == popFloatOrInt(intr)))},
		{">", intr => intr.push(new LangBool(popFloatOrInt(intr) < popFloatOrInt(intr)))},
		{"int?", intr => intr.push(new LangBool(intr.pop() is LangInt))},
		{":", define_word},
		{"def", define_word}, // synonym for :
		{".c", printchar},
		{"repr", intr => intr.push(new LangString(intr.pop().fmtStackPrint()))},
		{"str", intr => intr.push(new LangString(intr.pop().fmtDisplay()))},
		{"puts", _puts},
		{"depth", intr => intr.push(new LangInt(intr.SP_EMPTY - intr.SP))},
		{"SP", intr => intr.push(new LangInt(intr.SP))},
		{"SP!", setsp},
		{"LP", intr => intr.push(new LangInt(intr.LP))},
		{"LP!", setlp},
		{">L", tolocal},
		{"L>", fromlocal},
		{"set!", set},
		{"ref", _ref},
		{".showdef", showdef},
	};
}
