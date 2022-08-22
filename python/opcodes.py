from __future__ import annotations

# VM Opcodes - ported from C++ implementation
#
# Copyright (c) 2022 Frank McIngvale, see LICENSE

from errors import LangError

# C++ is the reference -- see docs there

# these values must match the C++ implementation!
OPCODE_FRAME_GET = 0
OPCODE_FRAME_SET = 1
OPCODE_JUMP_FORW = 2
OPCODE_JUMP_BACK = 3

def opcode_pack(code, A, B, C):
	# C is max 20 bits
	if C > 0x000fffff:
		raise LangError("C > 20 bits in opcode_pack()")

	return code | (A<<8) | (B<<16) | (C<<32)

# returns (code, A, B, C)
def opcode_unpack(packed):
	code = packed & 0x000000ff
	A = (packed >>  8) & 0x000000ff
	B = (packed >> 16) & 0x0000ffff
	C = (packed >> 32) & 0x000fffff

	return (code, A, B, C)

# --- opcode functions ---

# A = number of frames up (0 means my frame); B = index of var in outer frame
def opcode_FRAME_GET(intr, levels, index, _unused):
	if intr.framedata is None:
		raise LangError("opcode FRAME-GET called on null frame")

	intr.push(intr.framedata.getFrameObj(levels, index))

# A = number of frames up (0 means my frame); B = index of var in outer frame
def opcode_FRAME_SET(intr, levels, index, _unused):
	if intr.framedata is None:
		raise LangError("opcode FRAME-SET called on null frame")

	intr.framedata.setFrameObj(levels, index, intr.pop())
	
def _do_opcode_JUMP(intr, offset):
	if intr.code is None:
		raise LangError("JUMP with no running code?")

	pos = intr.codepos + offset
	if pos < 0 or pos >= len(intr.code):
		raise LangError("JUMP out of bounds")

	intr.codepos = pos

def opcode_JUMP_FORW(intr, _A, _B, offset):
	_do_opcode_JUMP(intr,offset)

def opcode_JUMP_BACK(intr, _A, _B, offset):
	_do_opcode_JUMP(intr,-offset)

# this is a list instead of dict for performance -- make sure ordering
# matches the constants order
OPCODE_FUNCTIONS = [
	opcode_FRAME_GET, opcode_FRAME_SET, opcode_JUMP_FORW, opcode_JUMP_BACK,
]

# these are not speed critical so can use maps so these are order-independent
NAME_TO_CODE = { 
	"FRAME-GET": OPCODE_FRAME_GET,
	"FRAME-SET": OPCODE_FRAME_SET,
	"JUMP-FORW": OPCODE_JUMP_FORW,
	"JUMP-BACK": OPCODE_JUMP_BACK,
}

CODE_TO_NAME = {
	OPCODE_FRAME_GET: "FRAME-GET",
	OPCODE_FRAME_SET: "FRAME-SET",
	OPCODE_JUMP_FORW: "JUMP-FORW",
	OPCODE_JUMP_BACK: "JUMP-BACK",
}

def opcode_name_to_code(name):
	if name not in NAME_TO_CODE:
		raise LangError("No such opcode name: " + str(name))

	return NAME_TO_CODE[name]

def opcode_code_to_name(code):
	if code not in CODE_TO_NAME:
		raise LangError("No such opcode number: " + str(code))

	return CODE_TO_NAME[code]
