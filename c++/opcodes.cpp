/*
	Interpreter opcodes **EXPERIMENTAL**

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "opcodes.hpp"
#include "errors.hpp"
#include "interpreter.hpp"
#include <map>
using namespace std;

uint64_t opcode_pack(uint8_t code, uint8_t A, uint16_t B, uint32_t C) {
	// C is max 20 bits
	if(C > 0x000fffff)
		throw LangError("C > 20 bits in opcode_pack()");

	return code | (A<<8) | (B<<16) | (((uint64_t)C)<<32);
}

void opcode_unpack(uint64_t opcode, uint8_t &code, uint8_t &A, uint16_t &B, uint32_t &C) {
	code = opcode & 0xff;
	A = (opcode >> 8) & 0xff;
	B = (opcode >> 16) & 0xffff;
	C = (opcode >> 32) & 0x000fffff;
}

// use maps so this is order-independent
static map<string,uint8_t> NAME_TO_CODE { 
	{"FRAME-GET", OPCODE_FRAME_GET },
	{"FRAME-SET", OPCODE_FRAME_SET },
};

static map<uint8_t,string> CODE_TO_NAME {
	{OPCODE_FRAME_GET, "FRAME-GET"},
	{OPCODE_FRAME_SET, "FRAME-SET"},
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

	if(levels == 0) // local frame
		intr->push(intr->cur_framedata->getLocalObj(index));
	else // outer frame
		intr->push(intr->cur_framedata->getOuterObj(levels, index));
}

// A = number of frames up (0 means my frame); B = index of var in outer frame
void opcode_FRAME_SET(Interpreter *intr, uint8_t levels, uint16_t index, uint32_t _unused) {
	if(!intr->cur_framedata)
		throw LangError("opcode FRAME-SET called on null frame");

	auto obj = intr->pop(); // get obj to be stored

	if(levels == 0) // local frame
		intr->cur_framedata->setLocalObj(index, obj);
	else // outer frame
		intr->cur_framedata->setOuterObj(levels, index, obj);
}

// make sure order is correct!
std::vector<opcode_func> OPCODE_FUNCTIONS {
	opcode_FRAME_GET, opcode_FRAME_SET };