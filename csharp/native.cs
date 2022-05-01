/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ version.
*/
using System;
using System.Collections.Generic;

class Builtins {
	public static LangList NATIVE_CMDLINE_ARGS = new LangList();
	public static bool ALLOW_OVERWRITING_WORDS = false;

	public static long popInt(Interpreter intr, string where) {
		var obj = intr.pop();
		var i = obj as LangInt;
		if(i == null) {
			throw new LangError("Expecting int (in " + where + ") but got: " + obj.fmtStackPrint());
		}
		return i.value;
	}

	public static string popString(Interpreter intr, string where) {
		var obj = intr.pop();
		var s = obj as LangString;
		if(s == null) {
			throw new LangError("Expecting string (in " + where + ") but got: " + obj.fmtStackPrint());
		}
		return s.value;
	}

	public static string popSymbol(Interpreter intr, string where) {
		var obj = intr.pop();
		var s = obj as LangSymbol;
		if(s == null) {
			throw new LangError("Expecting symbol (in " + where + ") but got: " + obj.fmtStackPrint());
		}
		return s.value;
	}

	public static string popStringOrSymbol(Interpreter intr, string where) {
		var obj = intr.pop();
		var s = obj as LangString;
		if(s != null) {
			return (obj as LangString)!.value;
		}
		var y = obj as LangSymbol;
		if(y != null) {
			return (obj as LangSymbol)!.value;
		}
		throw new LangError("Expecting string or symbol (in " + where + ") but got: " + obj.fmtStackPrint());
	}
		
	public static double popFloatOrInt(Interpreter intr, string where) {
		var obj = intr.pop();
		if (obj is LangInt) {
			return (double)((obj as LangInt)!.value);
		}
		else if (obj is LangFloat) {
			return (obj as LangFloat)!.value;
		}
		else {
			throw new LangError("Expecting int or float (in " + where + ") but got: " + obj.fmtStackPrint());
		}
	}

	public static LangList popList(Interpreter intr, string where) {
		var obj = intr.pop();
		var list = obj as LangList;
		if(list == null) {
			throw new LangError("Expecting list (in " + where + ") but got: " + obj.fmtStackPrint());
		}
		return list;
	}

	public static void add(Interpreter intr) {
		var b = intr.pop();
		var a = intr.pop();
		var a_i = a as LangInt;
		var b_i = b as LangInt;
		// keep as integers when possible
		if(a_i != null && b_i != null) {
			intr.push(new LangInt(a_i.value + b_i.value));
			return;
		}
		if(a.isNumeric() && b.isNumeric()) {
			intr.push(new LangFloat(a.asFloat() + b.asFloat()));
			return;
		}
		if((a is LangString) && (b is LangString)) {
			intr.push(new LangString(a.asStringLike() + b.asStringLike()));
			return;
		}
		if((a is LangSymbol) && (b is LangSymbol)) {
			intr.push(new LangSymbol(a.asStringLike() + b.asStringLike()));
			return;
		}
		var a_l = a as LangList;
		var b_l = b as LangList;
		if(a_l != null && b_l != null) {
			var newlist = new LangList();
			newlist.objlist.AddRange(a_l.objlist);
			newlist.objlist.AddRange(b_l.objlist);
			intr.push(newlist);
			return;
		}
		throw new LangError("Don't know how to add " + a.fmtStackPrint() + " and " + b.fmtStackPrint() +
				" (" + a.typename() + ", " + b.typename() + ")");
	}

	public static void subtract(Interpreter intr) {
		var b = intr.pop();
		var a = intr.pop();
		var a_i = a as LangInt;
		var b_i = b as LangInt;
		// keep as integers when possible
		if(a_i != null && b_i != null) {
			intr.push(new LangInt(a_i.value - b_i.value));
			return;
		}
		if(a.isNumeric() && b.isNumeric()) {
			intr.push(new LangFloat(a.asFloat() - b.asFloat()));
			return;
		}
		throw new LangError("Don't know how to subtract " + a.fmtStackPrint() + " and " + b.fmtStackPrint() +
				" (" + a.typename() + ", " + b.typename() + ")");
	}

	public static void multiply(Interpreter intr) {
		var b = intr.pop();
		var a = intr.pop();
		var a_i = a as LangInt;
		var b_i = b as LangInt;
		// keep as integers when possible
		if(a_i != null && b_i != null) {
			intr.push(new LangInt(a_i.value * b_i.value));
			return;
		}
		if(a.isNumeric() && b.isNumeric()) {
			intr.push(new LangFloat(a.asFloat() * b.asFloat()));
			return;
		}
		throw new LangError("Don't know how to multiply: " + a.fmtStackPrint() + " and " + b.fmtStackPrint() +
				" (" + a.typename() + ", " + b.typename() + ")");
	}

	public static void divide(Interpreter intr) {
		// like c++ implementation, this is ALWAYS a float result
		var b = intr.pop();
		var a = intr.pop();
		if(a.isNumeric() && b.isNumeric()) {
			if(b.asFloat() == 0) {
				throw new LangError("Divide by zero");
			}
			intr.push(new LangFloat(a.asFloat() / b.asFloat()));
			return;
		}
		throw new LangError("Don't know how to divide: " + a.fmtStackPrint() + " and " + b.fmtStackPrint() +
				" (" + a.typename() + ", " + b.typename() + ")");
	}

	// see C++ version for extensive comments on divmod
	public static void int_divmod(Interpreter intr) {
		var b = popInt(intr,"divmode");
		var a = popInt(intr,"divmod");
	
		if(b == 0) {
			throw new LangError("Divide by zero");
		}
		long quot = (long)Math.Floor(((double)(Math.Abs(a))) / ((double)(Math.Abs(b))));

		bool samesign = (a < 0 && b < 0) || (a >=0 && b >= 0);
		long mod;
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

	public static void printchar(Interpreter intr) {
		long c = popInt(intr,"printchar");
		char ch = (char)c;
		Console.Write(ch);
		if(c == 10 || c == 13) {
			Console.Out.Flush();
		}
	}

	// set stack pointer from addr on stack
	// (SP values must be integers)
	public static void setsp(Interpreter intr) {
		long addr = popInt(intr,"SP!");
		if(addr < intr.SP_MIN || addr > intr.SP_EMPTY) {
			throw new LangError("Bad address in SP!: " + addr.ToString());
		}
		// since the above test passed, I know this conversion is ok
		intr.SP = (int)addr;
		// stats
		intr.min_run_SP = Math.Min(intr.min_run_SP,intr.SP);
	}

	// set locals pointer from addr on stack
	// (LP values must be integers)
	public static void setlp(Interpreter intr) {
		long addr = popInt(intr,"LP!");
		if(addr < intr.LP_MIN || addr > intr.LP_EMPTY) {
			throw new LangError("Bad address in LP!: " + addr.ToString());
		}
		// since the above test passed, I know this conversion is ok
		intr.LP = (int)addr;
		// stats
		intr.min_run_LP = Math.Min(intr.min_run_LP,intr.LP);
	}

	// pop top of stack and push to locals
	public static void tolocal(Interpreter intr) {
		if(intr.LP <= intr.LP_MIN) {
			throw new LangError("Locals overflow");
		}
		intr.OBJMEM[--intr.LP] = intr.pop();
	}

	// pop top locals and push to stack
	public static void fromlocal(Interpreter intr) {
		if(intr.LP >= intr.LP_EMPTY) {
			throw new LangError("Locals underflow");
		}
		intr.push(intr.OBJMEM[intr.LP++]);
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
		if(addr_i != null) {
			// SP or LP index
			if(addr_i.value < 0 || addr_i.value >= intr.OBJMEM.Count) {
				throw new LangError("Bad address in set!: " + addr_i.value.ToString());
			}
			// i know this cast is safe due to test above
			intr.OBJMEM[(int)addr_i.value] = obj;
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
		if(addr_i != null) {
			if(addr_i.value < 0 || addr_i.value >= intr.OBJMEM.Count) {
				throw new LangError("Bad address in ref: " + addr_i.value.ToString());
			}
			// (int) is safe due to above check
			intr.push(intr.OBJMEM[(int)addr_i.value]);
		}
		else {
			throw new LangError("NOT IMPLEMENTED IN ref");
		}
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

	public static void read_file(Interpreter intr) {
		string filename = popString(intr,"read-file");
		var text = System.IO.File.ReadAllText(filename);
		intr.push(new LangString(text));
	}

	public static void make_list(Interpreter intr) {
		long nr = popInt(intr,"make-list");
		var list = new LangList();
		for(long i=0; i<nr; ++i) {
			list.objlist.Insert(0, intr.pop());
		}
		intr.push(list);
	}

	public static bool test_equal(LangObject a, LangObject b) {
	
		if(a is LangNull) { return b is LangNull; }
		else if(a.isNumeric()) { return b.isNumeric() && a.asFloat() == b.asFloat(); }
		else if(a is LangBool) { 
			return b is LangBool && ((a as LangBool).value == (b as LangBool).value);
		}
		else if(a is LangLambda) { return false; } // lambdas never equal to anything
		else if(a is LangString) { 
			return b is LangString && ((a as LangString).value == (b as LangString).value);
		}
		else if(a is LangSymbol) { 
			return b is LangSymbol && ((a as LangSymbol).value == (b as LangSymbol).value);
		}
		// lists are deep compared with test_equal() on each element
		else if(a is LangList) {
			var aList = a as LangList;
			var bList = b as LangList;
			// the aList check is to prevent a compiler warning below
			if(aList == null || bList == null)
				return false;

			if(aList.objlist.Count != bList.objlist.Count)
				return false;

			for(var i=0; i<aList.objlist.Count; ++i) {
				if(!test_equal(aList.objlist[i], bList.objlist[i]))
					return false;
			}
			return true;
		}
		else {
			throw new LangError("Don't know how to compare (==) objects: " + a.fmtStackPrint() + " and " + b.fmtStackPrint());
		}
	}

	public static void equal(Interpreter intr) {
		var b = intr.pop();
		var a = intr.pop();
		intr.push(new LangBool(test_equal(a,b)));
	}

	// unlike ==, here comparing objects of different types in an error
	public static void greater(Interpreter intr) {
		var b = intr.pop();
		var a = intr.pop();

		//Console.WriteLine("GREATER: " + a.fmtStackPrint() + " and " + b.fmtStackPrint());

		if(a.isNumeric() && b.isNumeric()) { intr.push(new LangBool(a.asFloat() > b.asFloat())); }
		else if(a is LangString && b is LangString) { 
			int v = String.CompareOrdinal((a as LangString)!.value, (b as LangString)!.value);
			intr.push(new LangBool(v > 0));
		}
		else if(a is LangSymbol && b is LangSymbol) { 
			int v = String.CompareOrdinal((a as LangSymbol)!.value, (b as LangSymbol)!.value);
			intr.push(new LangBool(v > 0));
		}
		else {
			throw new LangError("Cannot compare (>) objects: " + a.fmtStackPrint() + " and " + b.fmtStackPrint());
		}
	}
	
	public static void slice(Interpreter intr) {
		long nr = popInt(intr,"slice");
		long index = popInt(intr,"slice");
		var obj = intr.pop();
		if(!obj.hasLength()) {
			throw new LangError("Object does not support slicing: " + obj.fmtStackPrint());
		}
		int objsize = obj.getLength();

		// adjust index & nr for negative & out of bounds conditions
		if(index < 0) { // index < 0 means count from end
			index = objsize + index;
		}
		if(index < 0 || index >= objsize) { // out of bounds - return empty object
			if(obj is LangString) { intr.push(new LangString("")); return; }
			else if(obj is LangSymbol) { intr.push(new LangSymbol("")); return; }
			else if(obj is LangList) { intr.push(new LangList()); return; }
			else { throw new LangError("Should never happen!"); }
		}
		if(nr < 0) { // nr < 0 means "copy all, starting at index"
			nr = objsize - index;
		}
		if((index+nr) > objsize) { // past end of object, truncate
			nr = objsize - index;
		}
		// object sizes are limited to 32-bits
		intr.push(obj.getSlice((int)index,(int)nr));
	}
		
	public static void unmake(Interpreter intr) {
		var obj = intr.pop();
		if(obj.isStringLike()) {
			var s = obj.asStringLike();
			for(int i=0; i<s.Length; ++i) {
				intr.push(new LangInt(s[i]));
			}
			intr.push(new LangInt(s.Length));
		}
		else if(obj is LangList) {
			foreach(var o in (obj as LangList)!.objlist) {
				intr.push(o);
			}
			intr.push(new LangInt(obj.getLength()));
		}
		else if(obj is LangLambda) {
			// push a deepcopy - see DESIGN-NOTES.md
			intr.push(new LangList(LangList.deepcopyObjlist((obj as LangLambda)!.objlist)));
		}
		else if(obj is LangClosure) {
			// as above, deepcopy list. state is mutable, so do not deepcopy it
			intr.push(new LangList(LangList.deepcopyObjlist((obj as LangClosure)!.objlist)));
			intr.push((obj as LangClosure)!.state);
		}
		else {
			throw new LangError("Object cannot be unmade: " + obj.fmtStackPrint());
		}
	}

	public static void make_string(Interpreter intr) {
		long nr = popInt(intr,"make-string");
		string s = "";
		for(long i=0; i<nr; ++i) {
			s = (char)(popInt(intr,"make-string")) + s;
		}
		intr.push(new LangString(s));
	}

	public static void make_symbol(Interpreter intr) {
		long nr = popInt(intr,"make-symbol");
		string s = "";
		for(int i=0; i<nr; ++i) {
			s = (char)(popInt(intr,"make-symbol")) + s;
		}
		intr.push(new LangSymbol(s));
	}

	public static void length(Interpreter intr) {
		var obj = intr.pop();
		if(obj.hasLength()) {
			intr.push(new LangInt(obj.getLength()));
		}
		else {
			throw new LangError("Object does not support length method: " + obj.fmtStackPrint());
		}
	}

	public static void make_word(Interpreter intr) {
		var name = popSymbol(intr, "make-word");
		var list = popList(intr, "make-word");
		intr.defineWord(name, list.objlist, ALLOW_OVERWRITING_WORDS);
	}

	public static void append(Interpreter intr) {
		var obj = intr.pop();
		var list = popList(intr,"append");
		list.objlist.Add(obj);
		intr.push(list);
	}
		
	public static void make_lambda(Interpreter intr) {
		var list = popList(intr, "make-lambda");
		// deepcopy list - see DESIGN-NOTES.md
		intr.push(new LangLambda(LangList.deepcopyObjlist(list.objlist)));
	}
	
	public static void dumpword(Interpreter intr) {
		var symbol = popSymbol(intr,".dumpword");
		var list = intr.lookupWord(symbol);
		if(list == null)
			throw new LangError("No such word in .dumpword: " + symbol);
		else
			intr.push(new LangList(LangList.deepcopyObjlist(list)));
	}

	public static void make_closure(Interpreter intr) {
		var state = intr.pop();
		var objlist = popList(intr,"make-closure");
		// as above, deepcopy list
		intr.push(new LangClosure(LangList.deepcopyObjlist(objlist.objlist), state));
	}

	public static void self_get(Interpreter intr) {
		if(intr.closure == null)
			throw new LangError("Attempting to reference unbound self");
		else
			intr.push(intr.closure.state);
	}

	public static void self_set(Interpreter intr) {
		if(intr.closure == null)
			throw new LangError("Attempting to set unbound self");
		else
			intr.closure.state = intr.pop();
	}

	public static void self_put(Interpreter intr) {
		var obj = intr.pop();
		var index = popInt(intr,"put");
		var list = popList(intr,"put");
		if(index < 0 || index >= list!.objlist.Count)
			throw new LangError("Index out of bounds in put");
		// note list sizes are limited to 32-bits
		list.objlist[(int)index] = obj;
		intr.push(list);
	}
	
	public static Dictionary<string,Action<Interpreter>> builtins = 
		new Dictionary<string,Action<Interpreter>> { 
		{"+", add},
		{"-", subtract},
		{"*", multiply},
		{"/", divide},
		{"f.setprec", intr => LangFloat.FLOAT_PRECISION = (int)popInt(intr,"f.setprec")},
		{"/mod", int_divmod},
		{"==", equal},
		{">", greater},
		{"int?", intr => intr.push(new LangBool(intr.pop() is LangInt))},
		{"float?", intr => intr.push(new LangBool(intr.pop() is LangFloat))},
		{"bool?", intr => intr.push(new LangBool(intr.pop() is LangBool))},
		{"null?", intr => intr.push(new LangBool(intr.pop() is LangNull))},
		{"list?", intr => intr.push(new LangBool(intr.pop() is LangList))},
		{"string?", intr => intr.push(new LangBool(intr.pop() is LangString))},
		{"symbol?", intr => intr.push(new LangBool(intr.pop() is LangSymbol))},
		{"lambda?", intr => intr.push(new LangBool(intr.pop() is LangLambda))},
		{"null", intr => intr.push(new LangNull())},
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
		
		{"make-list", make_list},
		{"slice", slice},
		{"unmake", unmake},
		{"make-string", make_string},
		{"length", length},
		{"make-word", make_word},
		{"append", append},
		{"parse-int", intr => intr.push(new LangInt(long.Parse(popStringOrSymbol(intr,"parse-int"))))},
		{"parse-float", intr => intr.push(new LangFloat(double.Parse(popStringOrSymbol(intr,"parse-float"))))},
		{"make-lambda", make_lambda},
		{"make-symbol", make_symbol},
		{".dumpword", dumpword},
		{"error", intr => throw new LangError(popString(intr,"error")) },
		{"cmdline-args", intr => intr.push(NATIVE_CMDLINE_ARGS)},
		{"read-file", read_file},
		{"make-closure", make_closure},
		{"self", self_get},
		{"self!", self_set},
		{"put", self_put},
		{"deepcopy", intr => intr.push(intr.pop().deepcopy())},
		{"alloc", intr => intr.push(new LangInt(intr.heapAllocate(popInt(intr,"alloc"))))},
		{",,del", intr => intr.deleteWord(popSymbol(intr,",,del"))},
	};
}
