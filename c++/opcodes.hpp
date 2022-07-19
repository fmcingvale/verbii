/*
	Interpreter opcodes **EXPERIMENTAL**

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once
#include <stdint.h> // get the uint* types
#include <string>

/*
	Opcode packing -- to simplify/standardize the interface, the opcode
	is packed as follows:

	- Packed into uint64_t (Object.data.opcode)
	- Bits:
	- 7:0	Opcode
		- there are currently approx 84 native words. assuming ALL these became
			opcodes (which is highly unlikely), allowing up to 256 should be
			sufficient (probably i'll reserve some of the higher values to be
			page-selectors to allow expanding this further)
	- 15:8	8 bit data  ("A")
	- 31:16 16 bit data ("B")
	- 63:32	32 bit data ("C")
*/

// A = number of frames up (0 means my frame); B = index of var in outer frame
const int OPCODE_FRAME_GET = 0; 
// A = number of frames up (0 means my frame); B = index of var in outer frame
const int OPCODE_FRAME_SET = 1; 

uint64_t opcode_pack(uint8_t code, uint8_t A, uint16_t B, uint32_t C);

uint8_t opcode_getcode(uint64_t opcode);
uint8_t opcode_getA(uint64_t opcode);
uint16_t opcode_getB(uint64_t opcode);
uint32_t opcode_getC(uint64_t opcode);

// translate to/from opcode name
uint8_t opcode_name_to_code(const char *name);
const std::string& opcode_code_to_name(uint8_t code);




