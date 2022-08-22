
/*
	Interpreter opcodes

	Ported from C++ implementation. See documentation there.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#nullable enable

using System;
using System.IO;
using System.Collections.Generic;

public class Opcodes {
	// make sure these match c++ reference
	const byte OPCODE_FRAME_GET = 0; 
	const byte OPCODE_FRAME_SET = 1; 
	const byte OPCODE_JUMP_FORW = 2;
	const byte OPCODE_JUMP_BACK = 3;

	// see c++ notes for why this is a signed long return
	public static long opcode_pack(byte code, byte A, ushort B, uint C) {
		if(C > 0x000fffff)
			throw new LangError("C > 20 bits in opcode_pack()");

		return (long)code | ((long)A<<8) | ((long)B<<16) | (((long)C)<<32);
	}

	public static void opcode_unpack(long packed, out byte code, out byte A, out ushort B, out uint C) {
		code = (byte)(((ulong)packed) & 0xff);
		A = (byte)((((ulong)packed) >> 8) & 0xff);
		B = (ushort)((((ulong)packed) >> 16) & 0xffff);
		C = (uint)((((ulong)packed) >> 32) & 0x000fffff);
	}

	public static Dictionary<string,byte> NAME_TO_CODE = 
		new Dictionary<string,byte> { 
			{"FRAME-GET", OPCODE_FRAME_GET},
			{"FRAME-SET", OPCODE_FRAME_SET},
			{"JUMP-FORW", OPCODE_JUMP_FORW},
			{"JUMP-BACK", OPCODE_JUMP_BACK},
		};

	public static Dictionary<byte,string> CODE_TO_NAME = 
		new Dictionary<byte,string> { 
			{OPCODE_FRAME_GET, "FRAME-GET"},
			{OPCODE_FRAME_SET, "FRAME-SET"},
			{OPCODE_JUMP_FORW, "JUMP-FORW"},
			{OPCODE_JUMP_BACK, "JUMP-BACK"},
		};

	public static byte opcode_name_to_code(string name) {
		if(NAME_TO_CODE.ContainsKey(name))
			return NAME_TO_CODE[name];
		else
			throw new LangError("No such opcode name: " + name);
	}

	public static string opcode_code_to_name(byte code) {
		if(CODE_TO_NAME.ContainsKey(code))
			return CODE_TO_NAME[code];
		else
			throw new LangError("No such opcode number: " + code.ToString());
	}

	// opcode functions -- put into an array
	public static void op_FRAME_GET(Interpreter intr, byte levels, ushort index, uint _unused) {
		if(intr.framedata == null)
			throw new LangError("FRAME-GET called with null frame");
		
		intr.push(intr.framedata.getFrameObj(levels, index));
	}

	public static void op_FRAME_SET(Interpreter intr, byte levels, ushort index, uint _unused) {
		if(intr.framedata == null)
			throw new LangError("FRAME-GET called with null frame");
		
		var obj = intr.pop();
		intr.framedata.setFrameObj(levels, index, obj);
	}
	
	public static void do_op_JUMP(Interpreter intr, int offset) {
		if(intr.code == null)
			throw new LangError("JUMP called without code?");

		var pos = intr.codepos + offset;
		if(pos < 0 || pos >= intr.code.Count)
			throw new LangError("JUMP out of bounds");

		intr.codepos = pos;
	}

	public static void runOpcode(Interpreter intr, LangOpcode opcode) {
	
		switch(opcode.code) {
			case OPCODE_FRAME_GET:
				op_FRAME_GET(intr, opcode.A, opcode.B, opcode.C);
				break;

			case OPCODE_FRAME_SET:
				op_FRAME_SET(intr, opcode.A, opcode.B, opcode.C);
				break;
				
			case OPCODE_JUMP_FORW:
				do_op_JUMP(intr, (int)opcode.C);
				break;

			case OPCODE_JUMP_BACK:
				do_op_JUMP(intr, -((int)opcode.C));
				break;
				
			default:
				throw new LangError("Bad opcode: " + opcode.code.ToString());
		}
	}
}
