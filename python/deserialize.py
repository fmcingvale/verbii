from __future__ import annotations
from errors import LangError
from langtypes import LangLambda, LangString, isList, parseBool, LangNull, LangVoid, \
				LangOpcode
"""
	Deserialize - load bytecode from compiler and put into Interpreter.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ implementation.
"""

def deserialize_stream(intr, fileIn):
	line = fileIn.readline()
	if line == "":
		return LangVoid()

	line = line.rstrip() # remove \n
	if line[0] == 'i': return int(line[2:])
	elif line[0] == 'f': return float(line[2:])
	elif line[0] == 'b': return parseBool(line[2:])
	elif line[0] == 'n': return LangNull() 
	elif line[0] == 'o': return LangOpcode(int(line[2:]))
	elif line[0] == 's':
		s = line[2:]
		s = s.replace("%32"," ").replace("%09","\t").replace("%10","\n").replace("%13","\r").replace("%37","%")
		return LangString(s)
	elif line[0] == 'y': return line[2:]
	elif line[0] == 'L':
		nr = int(line[2:])
		objs = []
		for i in range(nr):
			objs.append(deserialize_stream(intr, fileIn))
		return objs
	elif line[0] == 'F': # lambda
		objs = deserialize_stream(intr, fileIn)
		if not isList(objs):
			raise LangError("Expecting list after 'F' but got: " + objs.fmtStackPrint())
		return LangLambda(objs)
	elif line[0] == 'W': # word
		name = line[2:]
		objs = deserialize_stream(intr, fileIn)
		if not isList(objs):
			raise LangError("Expecting list after 'W' but got: " + objs.fmtStackPrint())
		intr.defineWord(name,objs,False)
		#print("LOADED WORD: " + name)
		return LangVoid()
	else:
		raise LangError("Unrecognized line while deserializing: " + line)


