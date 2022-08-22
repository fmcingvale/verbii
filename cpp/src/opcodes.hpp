/*
	Interpreter opcodes

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#pragma once
#include <stdint.h> // get the uint* types
#include <string>
#include "interpreter.hpp"

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
	- 51:32	20 bit data ("C")
		- A,B,C are opcode-defined. so for example, if an opcode needed 32-bit
		  data it could put that in C:B for example.
		- packed opcode needs to fit into 52 bits to ensure it can be parsed
		  as an unsigned int on all platforms (i.e. when deserializing)
*/

// ** DO NOT CHANGE THE OPCODE NUMBERING -- these are synced across ports and 
// ** the ordering is relied on in several places. also the opcode values are
// ** compiled into the .b files so would break compiler/init/boot to change them.

// A = number of frames up (0 means my frame); B = index of var in outer frame
const int OPCODE_FRAME_GET = 0; 
// A = number of frames up (0 means my frame); B = index of var in outer frame
const int OPCODE_FRAME_SET = 1; 

// to simplify encoding/printing, make two opcodes for jumps -- one forward, one backward.
// using a bias value to encode +/- to a positive int would cause a bias value to have
// to be propogated to the print functions and into the verbii compiler. better to make
// opcodes generic and avoid special handling I think.

// C = jump offset
//	- implementation note -- when JUMP is called, codepos will be pointing to the
//	  position AFTER the jump opcode.
const int OPCODE_JUMP_FORW = 2;
// C = negative of offset
const int OPCODE_JUMP_BACK = 3;

// since the packed opcode is limited to 52 bits, a *signed* 64-bit value is
// used so that it is compatible with the native verbii integer type
int64_t opcode_pack(uint8_t code, uint8_t A, uint16_t B, uint32_t C);
void opcode_unpack(int64_t opcode, uint8_t &code, uint8_t &A, uint16_t &B, uint32_t &C);

// translate to/from opcode name
uint8_t opcode_name_to_code(const char *name);
const std::string& opcode_code_to_name(uint8_t code);

typedef void (*opcode_func)(Interpreter *intr, uint8_t A, uint16_t B, uint32_t C);

// indexed by OPCODE_*
extern std::vector<opcode_func> OPCODE_FUNCTIONS;



