/*
	LangTypes - type system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Not really like the other ports since I have to make a base
	Object so I can have heterogenous collections. C# apparently lacks
	unions and not sure if faking it with StructLayout mechanism is
	a great idea or not.
*/
#nullable enable
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

	// see c++ & DESIGN-NOTES.md
	public virtual LangObject deepcopy() { return this; } // MOST classes just return self
}

// in general, void is used in eof-type situations but can be used in any context
// where the code needs to differentiate a return value of null from a
// return of 'nothing'. for example the verbii compiler needs to be able to do this
// so that null can be a literal. void can never be a literal since the same problem
// would recur that there would be no way to differentiate a parsed void from eof.
public class LangVoid : LangObject {
	public LangVoid() {}

	// voids usually will not be present in stored data, so print it a little
	// differently so it is easy to spot ... might indicate a programming error etc.
	public override string typename() { return "<*void*>"; }
	public override string fmtDisplay() { return "<*void*>"; }
	public override string fmtStackPrint() { return fmtDisplay(); }
}

public class LangNull : LangObject {
	public LangNull() {}

	public override string typename() { return "null"; }
	public override string fmtDisplay() { return "<null>"; }
	public override string fmtStackPrint() { return fmtDisplay(); }
}

public class LangInt : LangObject {
	// see c++ notes
	const long MAX_VINT = (((long)1)<<53) - 1;
	const long MIN_VINT = -MAX_VINT;

	public LangInt(long i) {
		//if(i > MAX_VINT || i < MIN_VINT) {
		//	throw new LangError("Integer overflow");
		//}
		value = i;
	}

	public long value;

	public override string typename() { return "int"; }
	public override string fmtDisplay() { return value.ToString(); }
	public override string fmtStackPrint() { return fmtDisplay(); } 
	public override bool isNumeric() { return true; }
	public override double asFloat() { return value; }
}

public class LangOpcode : LangObject {
	// see c++ notes on packed format
	public LangOpcode(byte code, byte A, ushort B, uint C) { 
		this.code = code;
		this.A = A;
		this.B = B;
		this.C = C;
	}

	public byte code;
	public byte A;
	public ushort B;
	public uint C;

	public long packed() { return Opcodes.opcode_pack(code, A, B, C); }

	public override string typename() { return "opcode"; }
	public override string fmtDisplay() {
		return "#op( " + Opcodes.opcode_code_to_name(code) + " " + 
			A.ToString() + " " + B.ToString() + " " + C.ToString() + " )";
	}
	public override string fmtStackPrint() { return fmtDisplay(); }
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
		// seems to use 'E' instead of 'e' so make sure its lowercase
		return String.Format(fmt, value).ToLower();
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
	// NOTE: string size limited to 32-bits
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
	public override string fmtDisplay() { return value; }
	public override string fmtStackPrint() { return "'" + value; }

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
	public override string fmtDisplay() { 
		return LangList.fmtDisplayObjlist(objlist,"{","}");
	 }
	public override string fmtStackPrint() {
		return LangList.fmtStackPrintObjlist(objlist,"{","}");
	}
}

public class LangBoundLambda : LangObject {
	public List<LangObject> objlist;
	public CallFrameData? outer;

	public LangBoundLambda(LangLambda lambda, CallFrameData? outer) {
		this.objlist = lambda.objlist;
		this.outer = outer;
	}
	public override string typename() { return "bound-lambda"; }
	public override string fmtDisplay() { 
		return "<bound " + LangList.fmtDisplayObjlist(objlist,"{","}") + ">";
	 }
	public override string fmtStackPrint() {
		return "<bound " + LangList.fmtStackPrintObjlist(objlist,"{","}") + ">";
	 }
}

public class LangDict : LangObject {
	public LangDict() {
		dict = new SortedDictionary<String,LangObject>();
	}
	public SortedDictionary<String,LangObject> dict;
	public override string typename() { return "dict"; }
	public override string fmtDisplay() { 
		string s = "{ ";
		foreach(KeyValuePair<string,LangObject> pair in dict) {
			s += "\"" + pair.Key + "\" => " + pair.Value.fmtDisplay() + " ";
		}
		s += "}";
		return s;
	}
	public override string fmtStackPrint() { 
		string s = "{ ";
		foreach(KeyValuePair<string,LangObject> pair in dict) {
			s += "\"" + pair.Key + "\" => " + pair.Value.fmtStackPrint() + " ";
		}
		s += "}";
		return s;
	}

	public override bool hasLength() { return true; }
	public override int getLength() { return dict.Count; }

	public override LangObject deepcopy() {
		var d = new LangDict();
		foreach(KeyValuePair<string,LangObject> pair in dict)
			d.dict[pair.Key] = pair.Value;

		return d;
	}
}

public class LangList : LangObject {
	public LangList() {
		objlist = new List<LangObject>();
	}

	public LangList(List<LangObject> list) {
		objlist = list;
	}
	
	public List<LangObject> objlist;
	public override string typename() { return "list"; }

	public static string fmtDisplayObjlist(List<LangObject> objlist, string open_delim, string close_delim) {
		string s = open_delim;
		foreach(var obj in objlist) {
			s += " " + obj.fmtDisplay();
		}
		s += " " + close_delim;
		return s;
	}

	public override string fmtDisplay() {
		return fmtDisplayObjlist(objlist, "[", "]");
	}

	public static string fmtStackPrintObjlist(List<LangObject> objlist, string open_delim, string close_delim) {
		string s = open_delim;
		foreach(var obj in objlist) {
			s += " " + obj.fmtStackPrint();
		}
		s += " " + close_delim;
		return s;
	}

	public override string fmtStackPrint() {
		return fmtStackPrintObjlist(objlist, "[", "]");
	}

	public override bool hasLength() { return true; }
	public override int getLength() { return objlist.Count; }

	public override LangObject getSlice(int index, int nr) { 
		var list = new LangList();
		list.objlist = objlist.GetRange(index,nr);
		return list;
	}

	public static List<LangObject> deepcopyObjlist(List<LangObject> objlist) {
		var newlist = new List<LangObject>();
		foreach(var obj in objlist) {
			newlist.Add(obj.deepcopy());
		}
		return newlist;
	}

	public override LangObject deepcopy() {
		return new LangList(deepcopyObjlist(objlist));
	}
}

// NOTE this is a naive implementation -- later needs to be able to reuse
// frames that didn't get linked to anything instead of creating a new frame
// at each function call
public class CallFrameData {
	// *** SYNC THIS WITH C++ IMPLEMENTATION ***
	const int MAX_CALLFRAME_SLOTS = 255; 

	public CallFrameData() { 
		outer = null;
		data = new LangObject[MAX_CALLFRAME_SLOTS];
	}
	
	public void setOuterFrame(CallFrameData? _outer) { outer = _outer; }
	public CallFrameData? getOuterFrame() { return outer; }

	// get/set object in frame up #levels (0 == this frame)
	public LangObject getFrameObj(int levels, int index) {
		if(index < 0 || index >= MAX_CALLFRAME_SLOTS)
			throw new LangError("Out of bounds in CallFrameData::setLocalObj()");
		
		var frame = findFrameUp(levels);
		return frame.data[index];
	}

	public void setFrameObj(int levels, int index, LangObject obj) {
		if(index < 0 || index >= MAX_CALLFRAME_SLOTS)
			throw new LangError("Out of bounds in CallFrameData::setLocalObj()");

		var frame = findFrameUp(levels);
		frame.data[index] = obj;
	}

	private CallFrameData findFrameUp(int levels) {
		var frame = this;
		while(levels > 0) {
			if(frame == null || frame.outer == null)
				throw new LangError("Bad level number in findFrameUp()");

			levels -= 1;
			frame = frame.outer;
		}
		return frame; // cannot be NULL due to above checks
	}

	private LangObject[] data;
	private CallFrameData? outer;
}
