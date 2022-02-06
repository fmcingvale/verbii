/*
	LangTypes - type system.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Not really like the other ports since I have to make a base
	Object so I can have heterogenous collections. C# apparently lacks
	unions and not sure if faking it with StructLayout mechanism is
	a great idea or not.
*/

public class LangObject {
	public virtual string repr() { return "<VOID>"; }
}

public class LangInt : LangObject {
	public LangInt(int i) {
		value = i;
	}

	public int value;

	public override string repr() { return value.ToString(); } 
}

public class LangBool : LangObject {
	public LangBool(bool b) {
		value = b;
	}

	public bool value;

	public override string repr() { return value ? "true" : "false"; }
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
		offset = 0; // i have my own offset
	}

	public override string repr() { return "var:" + array.Count().ToString() + ":" + offset.ToString(); }
	
	public List<LangObject> array;
	public int offset;
}


