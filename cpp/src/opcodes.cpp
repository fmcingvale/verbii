/*
	Interpreter opcodes

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "opcodes.hpp"
#include "errors.hpp"
#include "interpreter.hpp"
#include <map>
using namespace std;

int64_t opcode_pack(uint8_t code, uint8_t A, uint16_t B, uint32_t C) {
	// C is max 20 bits
	if(C > 0x000fffff)
		throw LangError("C > 20 bits in opcode_pack()");

	return (int64_t)(code | (A<<8) | (B<<16) | (((int64_t)C)<<32));
}

void opcode_unpack(int64_t packed, uint8_t &code, uint8_t &A, uint16_t &B, uint32_t &C) {
	code = ((uint64_t)packed) & 0xff;
	A = (((uint64_t)packed) >> 8) & 0xff;
	B = (((uint64_t)packed) >> 16) & 0xffff;
	C = (((uint64_t)packed) >> 32) & 0x000fffff;
}

// use maps so this is order-independent
static map<string,uint8_t> NAME_TO_CODE { 
	{"FRAME-GET", OPCODE_FRAME_GET },
	{"FRAME-SET", OPCODE_FRAME_SET },
	{"JUMP-FORW", OPCODE_JUMP_FORW},
	{"JUMP-BACK", OPCODE_JUMP_BACK},
};

static map<uint8_t,string> CODE_TO_NAME {
	{OPCODE_FRAME_GET, "FRAME-GET"},
	{OPCODE_FRAME_SET, "FRAME-SET"},
	{OPCODE_JUMP_FORW, "JUMP-FORW"},
	{OPCODE_JUMP_BACK, "JUMP-BACK"},
};

uint8_t opcode_name_to_code(const char *name) {
	if(NAME_TO_CODE.find(name) == NAME_TO_CODE.end())
		throw LangError("No such opcode name: " + string(name));

	return NAME_TO_CODE[name];
}

const string& opcode_code_to_name(uint8_t code) {
	if(CODE_TO_NAME.find(code) == CODE_TO_NAME.end())
		throw LangError("No such opcode number: " + to_string(code));

	return CODE_TO_NAME[code];
}

// --- opcode functions ---

// A = number of frames up (0 means my frame); B = index of var in outer frame
void opcode_FRAME_GET(Interpreter *intr, uint8_t levels, uint16_t index, uint32_t _unused) {
	if(!intr->cur_framedata)
		throw LangError("opcode FRAME-GET called on null frame");

	intr->push(intr->cur_framedata->getFrameObj(levels, index));
}

// A = number of frames up (0 means my frame); B = index of var in outer frame
void opcode_FRAME_SET(Interpreter *intr, uint8_t levels, uint16_t index, uint32_t _unused) {
	if(!intr->cur_framedata)
		throw LangError("opcode FRAME-SET called on null frame");

	auto obj = intr->pop(); // get obj to be stored
	intr->cur_framedata->setFrameObj(levels, index, obj);
}

void do_opcode_JUMP(Interpreter *intr, int32_t offset) {
	if(!intr->code)
		throw LangError("opcode JUMP called but no code loaded??");

	auto newpos = intr->codepos + offset;
	if(newpos < 0 || newpos >= intr->code->size())
		throw LangError("JUMP out of bounds");

	intr->codepos = newpos;
}

void opcode_JUMP_FORW(Interpreter *intr, uint8_t _A, uint16_t _B, uint32_t offset) {
	do_opcode_JUMP(intr, offset);
}

void opcode_JUMP_BACK(Interpreter *intr, uint8_t _A, uint16_t _B, uint32_t offset) {
	do_opcode_JUMP(intr, -((int32_t)offset));
}

// make sure order matches ordering of values!
std::vector<opcode_func> OPCODE_FUNCTIONS {
	opcode_FRAME_GET, opcode_FRAME_SET, opcode_JUMP_FORW, opcode_JUMP_BACK };