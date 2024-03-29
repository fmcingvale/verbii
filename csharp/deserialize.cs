/*
	Deserialize - load bytecode produced by compiler.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ implementation.
*/
#nullable enable

using System;
using System.IO;

public class Deserializer {

	public static bool parseBool(string text) {
		if(text == "true")
			return true;
		else if(text == "false")
			return false;
		else
			throw new LangError("Bad boolean literal: " + text);
	}

	// deserialize & return next object from stream
	public static LangObject deserialize_stream(Interpreter intr, StreamReader stream) {
		string? line;
		while(true) {
			line = stream.ReadLine();
			if(line == null) {
				return new LangVoid();
			}
			// remove any \n or \r left on line
			while(line.Length > 0 && (line[line.Length-1] == '\n' || line[line.Length-1] == '\r'))
				line.Remove(line.Length-1);
			
			switch(line[0]) {
				case 'M':
					// ignore metadata and return NEXT object
					return deserialize_stream(intr, stream);
				case 'i': return new LangInt(long.Parse(line.Substring(2)));
				case 'f': return new LangFloat(double.Parse(line.Substring(2)));
				case 'b': return new LangBool(parseBool(line.Substring(2)));
				case 'n': return new LangNull();
				case 'o': 
					{ 
						byte code, A;
						ushort B;
						uint C;
						Opcodes.opcode_unpack(long.Parse(line.Substring(2)),
											out code, out A, out B, out C);
						return new LangOpcode(code, A, B, C);
					}
				case 's':
					line = line.Substring(2);
					line = line.Replace("%32", " ");
					line = line.Replace("%09", "\t");
					line = line.Replace("%10", "\n");
					line = line.Replace("%13", "\r");
					line = line.Replace("%37", "%");
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
							// do not allow overwriting words
							intr.defineWord(name, (list as LangList)!.objlist, false);
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

