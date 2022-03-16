/*
	Deserialize - load bytecode produced by compiler.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ implementation.
*/
#nullable enable

using System;
using System.IO;

public class Deserializer {

	// deserialize & return next object from stream
	public static LangObject deserialize_stream(Interpreter intr, StreamReader stream) {
		string? line;
		while(true) {
			line = stream.ReadLine();
			if(line == null) {
				return new LangVoid();
			}
			switch(line[0]) {
				case 'i': return new LangInt(int.Parse(line.Substring(2)));
				case 'f': return new LangFloat(double.Parse(line.Substring(2)));
				case 'n': return new LangNull();
				case 'b': return (line.Substring(2) == "true") ? new LangBool(true) : new LangBool(false);
				case 's':
					line = line.Substring(2);
					line = line.Replace("%32", " ");
					line = line.Replace("%10", "\n");
					line = line.Replace("%13", "\r");
					line = line.Replace("%%", "%");
					return new LangString(line);
				case 'y': return new LangSymbol(line.Substring(2));
				case 'L':
					{
						int nr = int.Parse(line.Substring(2));
						var list = new LangList();
						for(int i=0; i<nr; ++i) {
							list.objlist.Add(deserialize_stream(intr, stream));
						}
						return list;
					}
				case 'F':
					{
						LangObject list = deserialize_stream(intr, stream);
						if(list is LangList) {
							return new LangLambda((list as LangList)!.objlist);
						}
						else {
							throw new LangError("Expecting list after F but got: " + list.fmtStackPrint());
						}
					}
				case 'W':
					{
						string name = line.Substring(2);
						LangObject list = deserialize_stream(intr, stream);
						if(list is LangList) {
							intr.WORDS[name] = (list as LangList)!.objlist;
							return new LangVoid();
						}
						else {
							throw new LangError("Expecting list after W but got: " + list.fmtStackPrint());
						}
					}
				default:
					throw new LangError("Unrecognized line in deserialize: " + line);
			}
		}
	}
}

