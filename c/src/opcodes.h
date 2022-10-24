/*
	Interpreter opcodes

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#ifndef __opcodes_h__
#define __opcodes_h__

#include <stdint.h> // get the uint* types
// #include "interpreter.h"

// sync with C++

// A = number of frames up (0 means my frame); B = index of var in outer frame
#define OPCODE_FRAME_GET 0
// A = number of frames up (0 means my frame); B = index of var in outer frame
#define OPCODE_FRAME_SET 1

// C = jump offset
//	- implementation note -- when JUMP is called, codepos will be pointing to the
//	  position AFTER the jump opcode.
#define OPCODE_JUMP_FORW 2
// C = negative of offset
#define OPCODE_JUMP_BACK 3

// call builtin function
// A = builtin index (for call_builtin_by_index)
//
// this is a runtime optimization; this opcode is not emitted from the compiler
#define OPCODE_CALL_BUILTIN 4

// the CURRENT last OPCODE_* number + 1
#define OPCODE_LAST_PLUS1 5

// since the packed opcode is limited to 52 bits, a *signed* 64-bit value is
// used so that it is compatible with the native verbii integer type
int64_t opcode_pack(uint8_t code, uint8_t A, uint16_t B, uint32_t C);
void opcode_unpack(int64_t opcode, uint8_t *code, uint8_t *A, uint16_t *B, uint32_t *C);

// translate to/from opcode name
uint8_t opcode_name_to_code(const char *name);
const char* opcode_code_to_name(uint8_t code);

typedef void (*opcode_func)(uint8_t A, uint16_t B, uint32_t C);

// indexed by OPCODE_*
extern opcode_func OPCODE_FUNCTIONS[];

#endif // __opcodes_h__