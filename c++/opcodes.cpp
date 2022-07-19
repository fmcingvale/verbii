/*
	Interpreter opcodes **EXPERIMENTAL**

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "opcodes.hpp"
#include "errors.hpp"
#include <map>
using namespace std;

uint64_t opcode_pack(uint8_t code, uint8_t A, uint16_t B, uint32_t C) {
	return code | (A<<8) | (B<<16) | (((uint64_t)C)<<32);
}

uint8_t opcode_getcode(uint64_t opcode) { return opcode & 0xff; }
uint8_t opcode_getA(uint64_t opcode) { return (opcode >> 8) & 0xff; }
uint16_t opcode_getB(uint64_t opcode) { return (opcode >> 16) & 0xffff; }
uint32_t opcode_getC(uint64_t opcode) { return (opcode >> 32) & 0xffffffff; }

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
