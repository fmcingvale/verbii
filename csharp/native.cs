/*
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ version.
*/
using System;
using System.Collections.Generic;
using System.IO;

class Builtins {
	public static bool ALLOW_OVERWRITING_WORDS = false;
	public static bool EXIT_ON_EXCEPTION = true;
	public static bool STACKTRACE_ON_EXCEPTION = true;
	public static long STARTUP_TIME_MSEC = DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond;
	public static StreamWriter FP_STDOUT = StreamWriter.Null;

	public static long popInt(Interpreter intr, string where) {
		var obj = intr.pop();
		var i = obj as LangInt;
		if(i == null) {
			throw new LangError("Expecting int (in " + where + ") but got: " + obj.fmtStackPrint());
		}
		return i.value;
	}

	public static bool popBool(Interpreter intr, string where) {
		var obj = intr.pop();
		var b = obj as LangBool;
		if(b == null) {
			throw new LangError("Expecting bool (in " + where + ") but got: " + obj.fmtStackPrint());
		}
		return b.value;
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

	public static LangOpcode popOpcode(Interpreter intr, string where) {
		var obj = intr.pop();
		var op = obj as LangOpcode;
		if(op == null) {
			throw new LangError("Expecting opcode (in " + where + ") but got: " + obj.fmtStackPrint());
		}
		return op;
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

	public static LangLambda popLambda(Interpreter intr, string where) {
		var obj = intr.pop();
		var l = obj as LangLambda;
		if(l == null) {
			throw new LangError("Expecting lambda (in " + where + ") but got: " + obj.fmtStackPrint());
		}
		return l;
	}

	public static LangDict popDict(Interpreter intr, string where) {
		var obj = intr.pop();
		var d = obj as LangDict;
		if(d == null)
			throw new LangError("Expecting dict (in " + where + ") but got: " + obj.fmtStackPrint());
		else
			return d;
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
		if(FP_STDOUT == StreamWriter.Null) {
			Console.Write(ch);
			if(c == 10 || c == 13) {
				Console.Out.Flush();
			}
		}
		else {
			FP_STDOUT.Write(ch);
			if(c == 10 || c == 13) 
				FP_STDOUT.Flush();
		}
	}

	public static void open_as_stdout(Interpreter intr) {
		if(FP_STDOUT != StreamWriter.Null) {
			FP_STDOUT.Close();
		}
		var obj = intr.pop();
		if(obj is LangVoid)
			FP_STDOUT = StreamWriter.Null;
		else if(obj is LangString)
			FP_STDOUT = new StreamWriter((obj as LangString)!.value);
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
			if(FP_STDOUT == StreamWriter.Null)
				Console.Write(s.value);
			else
				FP_STDOUT.Write(s.value);
		}
	}

	public static void file_read(Interpreter intr) {
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
			return b is LangBool && ((a as LangBool)!.value == (b as LangBool)!.value);
		}
		else if(a is LangLambda) { return false; } // lambdas never equal to anything
		else if(a is LangBoundLambda) { return false; } // same for bound lambdas
		else if(a is LangString) { 
			return b is LangString && ((a as LangString)!.value == (b as LangString)!.value);
		}
		else if(a is LangSymbol) { 
			return b is LangSymbol && ((a as LangSymbol)!.value == (b as LangSymbol)!.value);
		}
		else if(a is LangVoid)
			return b is LangVoid;
		else if(a is LangOpcode) {
			return b is LangOpcode && 
				((a as LangOpcode)!.code == (b as LangOpcode)!.code) &&
				((a as LangOpcode)!.A == (b as LangOpcode)!.A) &&
				((a as LangOpcode)!.B == (b as LangOpcode)!.B) &&
				((a as LangOpcode)!.C == (b as LangOpcode)!.C);
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
		// similar for Dict - make sure they have the same keys & then test values recursively
		else if(a is LangDict) {
			var aDict = a as LangDict;
			var bDict = b as LangDict;
			if(aDict == null || bDict == null)
				return false;

			if(aDict.getLength() != bDict.getLength())
				return false;

			// 
			foreach(KeyValuePair<string,LangObject> pair in aDict.dict) {
				if(!bDict.dict.ContainsKey(pair.Key))
					return false;

				if(!test_equal(pair.Value,bDict.dict[pair.Key]))
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
	public static bool test_greater(LangObject a, LangObject b) {
		if(a.isNumeric() && b.isNumeric()) { return a.asFloat() > b.asFloat(); }
		else if(a is LangString && b is LangString) { 
			int v = String.CompareOrdinal((a as LangString)!.value, (b as LangString)!.value);
			return v > 0;
		}
		else if(a is LangSymbol && b is LangSymbol) { 
			int v = String.CompareOrdinal((a as LangSymbol)!.value, (b as LangSymbol)!.value);
			return v > 0;
		}
		else if(a is LangList && b is LangList) {
			// see c++ notes
			var oa = (a as LangList)!;
			var ob = (b as LangList)!;
			var nr = Math.Min(oa.objlist.Count, ob.objlist.Count);
			for(var i=0; i<nr; ++i) {
				if(test_greater(oa.objlist[i], ob.objlist[i]))
					return true;
				else if(!test_equal(oa.objlist[i], ob.objlist[i]))
					return false; // a[i] < b[i]
			}
			// nr elements are equal, so now check lengths
			return oa.objlist.Count > ob.objlist.Count;
		}
		else {
			throw new LangError("Cannot compare (>) objects: " + a.fmtStackPrint() + " and " + b.fmtStackPrint());
		}
	}

	public static void greater(Interpreter intr) {
		var b = intr.pop();
		var a = intr.pop();

		//Console.WriteLine("GREATER: " + a.fmtStackPrint() + " and " + b.fmtStackPrint());
		intr.push(new LangBool(test_greater(a,b)));
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

	public static void extend(Interpreter intr) {
		var src = popList(intr,"extend");
		var list = popList(intr,"extend");
		list.objlist.AddRange(src.objlist);
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

	public static void obj_get(Interpreter intr) {
		var indexOrKey = intr.pop();
		var obj = intr.pop();
		if(obj is LangString) {
			if(indexOrKey is LangInt) {
				var index = (indexOrKey as LangInt)!.value;
				var s = (obj as LangString)!.value;
				if(index < 0) index += s.Length; // index<0 counts from end
				if(index < 0 || index >= s.Length)
					intr.push(new LangVoid()); // out of bounds == void
				else
					// assumes length is limited to 32 bits
					intr.push(new LangString(s.Substring((int)index,1)));
			}
			else
				throw new LangError("Expecting index in get but got: " + indexOrKey.fmtStackPrint());
		}
		else if(obj is LangSymbol) {
			if(indexOrKey is LangInt) {
				var index = (indexOrKey as LangInt)!.value;
				var s = (obj as LangSymbol)!.value;
				if(index < 0) index += s.Length; // index<0 counts from end
				if(index < 0 || index >= s.Length)
					intr.push(new LangVoid()); // out of bounds == void
				else
					// assumes length is limited to 32 bits
					intr.push(new LangSymbol(s.Substring((int)index,1)));
			}
			else
				throw new LangError("Expecting index in get but got: " + indexOrKey.fmtStackPrint());
		}
		else if(obj is LangList) {
			if(indexOrKey is LangInt) {
				var index = (indexOrKey as LangInt)!.value;

				var list = (obj as LangList)!;
				if(index < 0) index += list.objlist.Count; // index<0 counts from end
				if(index < 0 || index >= list.objlist.Count)
					intr.push(new LangVoid()); // out of bounds == void
				else
					// note list sizes are limited to 32-bits
					intr.push(list.objlist[(int)index]);
			}
			else
				throw new LangError("Expecting index in put but got: " + indexOrKey.fmtStackPrint());
		}
		else if(obj is LangDict) {
			if(indexOrKey is LangString) {
				var dict = (obj as LangDict)!;
				var key = (indexOrKey as LangString)!.value;
				
				if(dict.dict.ContainsKey(key))
					intr.push(dict.dict[key]);
				else
					intr.push(new LangVoid()); // void if key doesn't exist
			}
			else
				throw new LangError("Expecting string key but got: " + indexOrKey.fmtStackPrint());
		}
		else
			throw new LangError("Object does not support put: " + obj.fmtStackPrint());
	}
	
	public static void obj_put(Interpreter intr) {
		var obj = intr.pop();
		var indexOrKey = intr.pop();
		var dest = intr.pop();
		if(dest is LangList) {
			if(indexOrKey is LangInt) {
				var index = (indexOrKey as LangInt)!.value;

				var list = (dest as LangList)!;
				if(index < 0) index += list.objlist.Count; // negative indexes count from end
				if(index < 0 || index >= list.objlist.Count)
					throw new LangError("Index out of bounds in put");

				// note list sizes are limited to 32-bits
				list.objlist[(int)index] = obj;
				intr.push(dest);
			}
			else
				throw new LangError("Expecting index in put but got: " + indexOrKey.fmtStackPrint());
		}
		else if(dest is LangDict) {
			if(indexOrKey is LangString) {
				var dict = (dest as LangDict)!;
				var key = (indexOrKey as LangString)!.value;
				
				// note that [] allows overwriting, but Add() does not
				dict.dict[key] = obj;
				intr.push(dest);
			}
			else
				throw new LangError("Expecting string key but got: " + indexOrKey.fmtStackPrint());
		}
		else
			throw new LangError("Object does not support put: " + dest.fmtStackPrint());
	}
	
	public static void bit_shl(Interpreter intr) {
		int nr = (int)popInt(intr,"bit-shl");
		var a = popInt(intr,"bit-shl");
		
		intr.push(new LangInt((a << nr) & 0xffffffff));
	}

	public static void bit_shr(Interpreter intr) {
		int nr = (int)popInt(intr,"bit-shr");
		var a = popInt(intr,"bit-shr");
		
		intr.push(new LangInt((a >> nr) & 0xffffffff));
	}

	public static void deserialize(Interpreter intr) {
		var fileIn = System.IO.File.OpenText(popString(intr,"deserialize"));
		Deserializer.deserialize_stream(intr, fileIn);
	}
		
	public static void prompt(Interpreter intr) {
		Console.Write(popString(intr,"prompt"));
		Console.Out.Flush();
		var line = Console.In.ReadLine();
		if(line == null)
			intr.push(new LangVoid());
		else
			intr.push(new LangString(line));
	}

	public static void time_string(Interpreter intr) {
		var now = DateTime.Now;
		var s = String.Format("{0:yyyy-mm-dd HH:mm:ss}", now);
		intr.push(new LangString(s));
	}

	public static void file_write(Interpreter intr) {
		var text = popString(intr,"file-write");
		var filename = popString(intr,"file-write");
		var fout = new System.IO.StreamWriter(filename, false);
		fout.Write(text);
		fout.Close();
	}

	public static void file_append(Interpreter intr) {
		var text = popString(intr,"file-append");
		var filename = popString(intr,"file-append");
		var fout = new System.IO.StreamWriter(filename, true);
		fout.Write(text);
		fout.Close();
	}

	public static void file_delete(Interpreter intr) {
		var filename = popString(intr,"file-delete");
		if(File.Exists(filename))
			File.Delete(filename);
	}

	public static void keys(Interpreter intr) {
		var dict = popDict(intr,"keys");
		var list = new LangList();
		foreach(KeyValuePair<string,LangObject> pair in dict.dict) {
			// no ordering requirement
			list.objlist.Add(new LangString(pair.Key));
		}
		intr.push(list);
	}

	public static void make_opcode(Interpreter intr) {
		var C = popInt(intr,"make-opcode");
		var B = popInt(intr,"make-opcode");
		var A = popInt(intr,"make-opcode");
		var name = popStringOrSymbol(intr,"make-opcode");

		// range checks
		if(A < 0 || A > 255)
			throw new LangError("A must be [0-255] in make-opcode, got: " + A.ToString());

		if(B < 0 || B > 65535)
			throw new LangError("B must be [0-65535] in make-opcode, got: " + B.ToString());

		if(C < 0 || C > 0x000fffff)
			throw new LangError("C must be [0-1048575] in make-opcode, got: " + C.ToString());

		intr.push(new LangOpcode(Opcodes.opcode_name_to_code(name), (byte)A, (ushort)B, (uint)C));
	}		

	public static void opcode_packed(Interpreter intr) {
		LangOpcode op = popOpcode(intr,"opcode-packed");
		intr.push(new LangInt(op.packed()));
	}

	public static void bind_lambda(Interpreter intr) {
		var lambda = popLambda(intr, "bind-lambda");
		// remember currently active frame -- when bound-lambda is called
		// later, this frame will be set as its outer frame
		intr.push(new LangBoundLambda(lambda, intr.framedata));
		// mark current frame as being linked now so it isn't freed
		// TODO
		// intr->cur_framedata->setLinked(true);
	}

	public static void getcwd(Interpreter intr) {
		var name = Directory.GetCurrentDirectory();
		intr.push(new LangString(name));
	}
	
	public static void wordlist(Interpreter intr) {
		intr.push(intr.getWordlist());
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
		{"void?", intr => intr.push(new LangBool(intr.pop() is LangVoid))},
		{"list?", intr => intr.push(new LangBool(intr.pop() is LangList))},
		{"string?", intr => intr.push(new LangBool(intr.pop() is LangString))},
		{"symbol?", intr => intr.push(new LangBool(intr.pop() is LangSymbol))},
		{"lambda?", intr => intr.push(new LangBool(intr.pop() is LangLambda))},
		{"bound-lambda?", intr => intr.push(new LangBool(intr.pop() is LangBoundLambda))},
		{"opcode?", intr => intr.push(new LangBool(intr.pop() is LangOpcode))},
		{"void", intr => intr.push(new LangVoid())},
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
		{"extend", extend},
		{"parse-int", intr => intr.push(new LangInt(long.Parse(popStringOrSymbol(intr,"parse-int"))))},
		{"parse-float", intr => intr.push(new LangFloat(double.Parse(popStringOrSymbol(intr,"parse-float"))))},
		{"make-lambda", make_lambda},
		{"make-symbol", make_symbol},
		{".dumpword", dumpword},
		{".wordlist", wordlist},
		{"error", intr => throw new LangError(popString(intr,"error")) },
		{"put", obj_put},
		{"get", obj_get},
		{"deepcopy", intr => intr.push(intr.pop().deepcopy())},
		{"alloc", intr => intr.push(new LangInt(intr.heapAllocate(popInt(intr,"alloc"))))},
		{",,del", intr => intr.deleteWord(popSymbol(intr,",,del"))},
		{"bit-and", intr => intr.push(new LangInt((((uint)popInt(intr,"bit-and"))&((uint)popInt(intr,"bit-and"))) & 0xffffffff))},
		{"bit-or", intr => intr.push(new LangInt((((uint)popInt(intr,"bit-or"))|((uint)popInt(intr,"bit-or"))) & 0xffffffff))},
		{"bit-xor", intr => intr.push(new LangInt((((uint)popInt(intr,"bit-xor"))^((uint)popInt(intr,"bit-xor"))) & 0xffffffff))},
		{"bit-not", intr => intr.push(new LangInt((~(uint)popInt(intr,"bit-not")) & 0xffffffff))},
		{"bit-shl", bit_shl},
		{"bit-shr", bit_shr},

		{"run-time",
			intr => intr.push(new LangFloat(((DateTime.Now.Ticks / TimeSpan.TicksPerMillisecond) - Builtins.STARTUP_TIME_MSEC) / 1000.0))},
		{",,new-dict", intr => intr.push(new LangDict())},

		{"file-exists?", intr => intr.push(new LangBool(File.Exists(popString(intr,"file-exists?"))))},
		{"file-mtime", intr => intr.push(new LangInt(File.GetLastWriteTime(popString(intr,"file-mtime")).Ticks))},
		{"set-allow-overwrite-words", intr => ALLOW_OVERWRITING_WORDS = popBool(intr,"set-allow-overwrite-words")},
		{"set-exit-on-exception", intr => EXIT_ON_EXCEPTION = popBool(intr,"set-exit-on-exception")},
		{"set-stacktrace-on-exception", intr => STACKTRACE_ON_EXCEPTION = popBool(intr,"set-stacktrace-on-exception")},
		{"deserialize", deserialize},
		{"prompt", prompt},
		{"open-as-stdout", open_as_stdout},

		{"time-string", time_string},
		{"floor", intr => intr.push(new LangInt((long)Math.Floor(popFloatOrInt(intr,"floor"))))},

		{"file-write", file_write},
		{"file-append", file_append},
		{"file-read", file_read},
		{"file-delete", file_delete},
			
		// can't find a (portable) way to get more platform info ...
		{"sys-platform", intr => intr.push(new LangString("C#"))},

		{"keys", keys},

		{"sin", intr => intr.push(new LangFloat(Math.Sin(popFloatOrInt(intr,"sin"))))},
		{"cos", intr => intr.push(new LangFloat(Math.Cos(popFloatOrInt(intr,"cos"))))},
		{"sqrt", intr => intr.push(new LangFloat(Math.Sqrt(popFloatOrInt(intr,"sqrt"))))},
		{"log", intr => intr.push(new LangFloat(Math.Log(popFloatOrInt(intr,"log"))))},

		{"make-opcode", make_opcode},
		{"opcode-packed", opcode_packed},
		{"bind-lambda", bind_lambda},

		{"file-pathsep", intr => intr.push(new LangString(new string(Path.DirectorySeparatorChar, 1)))},
		{"os-getcwd", getcwd},
	};
}
