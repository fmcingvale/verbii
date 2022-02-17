/*
	LangTypes - type system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Not really like the other ports since I have to make a base
	Object so I can have heterogenous collections. C# apparently lacks
	unions and not sure if faking it with StructLayout mechanism is
	a great idea or not.
*/
using System;
using System.Collections.Generic;

public class LangObject {
	public virtual string fmtDisplay() { return "<VOID>"; }
	public virtual string fmtStackPrint() { return "<VOID>"; }
}

public class LangNull : LangObject {
	public LangNull() {
	}

	public override string fmtDisplay() { return "<null>"; }
	public override string fmtStackPrint() { return fmtDisplay(); }
}

public class LangInt : LangObject {
	// integers only allowed to use 31 bits, to be portable across host languages
	const int MAX_INT_31 = (1<<30) - 1;
	const int MIN_INT_31 = -MAX_INT_31;

	public LangInt(int i) {
		if(i > MAX_INT_31 || i < MIN_INT_31) {
			throw new LangError("Integer overflow");
		}
		value = i;
	}

	public int value;

	public override string fmtDisplay() { return value.ToString(); }
	public override string fmtStackPrint() { return fmtDisplay(); } 
}

public class LangFloat : LangObject {
	public LangFloat(double d) {
		value = d;
	}

	public static int FLOAT_PRECISION = 17;

	public double value;

	public override string fmtDisplay() { 
		// there has to be a better way .....
		string fmt = "{0:G" + FLOAT_PRECISION.ToString() + "}"; 
		return String.Format(fmt, value); 
	}

	public override string fmtStackPrint() { 
		return "#" + fmtDisplay();
	}
}

public class LangBool : LangObject {
	public LangBool(bool b) {
		value = b;
	}

	public bool value;

	public override string fmtDisplay() { return value ? "true" : "false"; }
	public override string fmtStackPrint() { return fmtDisplay(); }
}

public class LangString : LangObject {
	public LangString(string s) {
		value = s;
	}

	public string value;

	public override string fmtDisplay() { return value; }
	public override string fmtStackPrint() { return "\"" + value + "\""; }
}

public class LangSymbol : LangObject {
	public LangSymbol(string s) {
		value = s;
	}

	public string value;

	// compare symbol to name. if nr>0 then only match first nr chars of symbol.
	public bool match(string name, int nr=0) { 
		if(nr == 0) {
			return value == name;
		}
		else {
			return value.Length >= nr && value.Substring(0, nr) == name;
		}
	}

	public override string fmtDisplay() { return "'" + value; }
	public override string fmtStackPrint() { return value; }
}

// like in the C++ version, memory allocations are separate objects instead of
// my original idea of having a contiguous MEMORY array in the interpreter.
// using separate objects is much better for GC than an infinitely growing MEMORY

public class LangMemoryArray : LangObject {
	public LangMemoryArray(int count) {
		array = new List<LangObject>(count);
		// prefill memory with zeros
		for(int i=0; i<count; ++i) {
			array.Add(new LangInt(0));
		}
		offset = 0;
	}

	// init as shallow copy of other, but with my own offset
	public LangMemoryArray(LangMemoryArray other) {
		array = other.array;
		offset = other.offset;
	}

	public override string fmtDisplay() { return "var:" + array.Count.ToString() + ":" + offset.ToString(); }
	public override string fmtStackPrint() { return fmtDisplay(); }

	public List<LangObject> array;
	public int offset;
}

public class LangLambda : LangObject {
	public LangLambda(List<LangObject> objlist) {
		this.objlist = objlist;
	}

	public List<LangObject> objlist;
	public override string fmtDisplay() { return "<lambda>"; }
	public override string fmtStackPrint() { return fmtDisplay(); }
}
