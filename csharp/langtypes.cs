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

public abstract class LangObject {
	public abstract string typename();
	// see c++ comments for display vs. stack format
	public abstract string fmtDisplay();
	public abstract string fmtStackPrint();

	public virtual bool isNumeric() { return false; }
	public virtual double asFloat() { return 0.0; }

	public virtual bool isStringLike() { return false; }
	public virtual string asStringLike() { return ""; }

	public virtual bool hasLength() { return false; }
	public virtual int getLength() { return 0; }

	// guarantees:
	//	- only called on objects where hasLength() == true
	//	- index and index+nr will be valid indexes
	public virtual LangObject getSlice(int index, int nr) { return new LangVoid(); }
}

// void is differentiated from null since null can be a valid language object.
// void is used internally only to mean "nothing, not even null"
public class LangVoid : LangObject {
	public LangVoid() {}

	public override string typename() { return "<VOID>"; }
	public override string fmtDisplay() { return "<VOID>"; }
	public override string fmtStackPrint() { return fmtDisplay(); }
}

public class LangNull : LangObject {
	public LangNull() {}

	public override string typename() { return "null"; }
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

	public override string typename() { return "int"; }
	public override string fmtDisplay() { return value.ToString(); }
	public override string fmtStackPrint() { return fmtDisplay(); } 
	public override bool isNumeric() { return true; }
	public override double asFloat() { return value; }
}

public class LangFloat : LangObject {
	public LangFloat(double d) {
		value = d;
	}

	public static int FLOAT_PRECISION = 17;

	public double value;

	public override string typename() { return "float"; }
	public override string fmtDisplay() { 
		// there has to be a better way .....
		string fmt = "{0:G" + FLOAT_PRECISION.ToString() + "}"; 
		return String.Format(fmt, value); 
	}

	public override string fmtStackPrint() { 
		return "#" + fmtDisplay();
	}
	public override bool isNumeric() { return true; }
	public override double asFloat() { return value; }
}

public class LangBool : LangObject {
	public LangBool(bool b) {
		value = b;
	}

	public bool value;

	public override string typename() { return "bool"; }
	public override string fmtDisplay() { return value ? "true" : "false"; }
	public override string fmtStackPrint() { return value ? "<true>" : "<false>"; }
}

public class LangString : LangObject {
	public LangString(string s) {
		value = s;
	}

	public string value;

	public override string typename() { return "string"; }
	public override string fmtDisplay() { return value; }
	public override string fmtStackPrint() { return "\"" + value + "\""; }

	public override bool hasLength() { return true; }
	public override int getLength() { return value.Length; }
	public override LangObject getSlice(int index, int nr) { return new LangString(value.Substring(index, nr)); }

	public override bool isStringLike() { return true; }
	public override string asStringLike() { return value; }
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

	public override string typename() { return "symbol"; }
	public override string fmtDisplay() { return "'" + value; }
	public override string fmtStackPrint() { return value; }

	public override bool hasLength() { return true; }
	public override int getLength() { return value.Length; }

	public override LangObject getSlice(int index, int nr) { return new LangSymbol(value.Substring(index, nr)); }

	public override bool isStringLike() { return true; }
	public override string asStringLike() { return value; }
}

public class LangLambda : LangObject {
	public LangLambda(List<LangObject> objlist) {
		this.objlist = objlist;
	}

	public List<LangObject> objlist;
	public override string typename() { return "lambda"; }
	public override string fmtDisplay() { return "<lambda>"; }
	public override string fmtStackPrint() { return fmtDisplay(); }
}

public class LangList : LangObject {
	public LangList() {
		objlist = new List<LangObject>();
	}

	public List<LangObject> objlist;
	public override string typename() { return "list"; }
	public override string fmtDisplay() {
		return fmtStackPrint(); // match c++ port behavior and use stack format for both
	}

	public override string fmtStackPrint() {
		string s = "[";
		foreach(var obj in objlist) {
			s += " " + obj.fmtStackPrint();
		}
		s += " ]";
		return s;
	}

	public override bool hasLength() { return true; }
	public override int getLength() { return objlist.Count; }

	public override LangObject getSlice(int index, int nr) { 
		var list = new LangList();
		list.objlist = objlist.GetRange(index,nr);
		return list;
	}
}
