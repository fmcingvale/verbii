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


