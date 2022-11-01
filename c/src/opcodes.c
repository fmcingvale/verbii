/*
	Interpreter opcodes

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/
#include "opcodes.h"
#include "errors.h"
#include "interpreter.h"
#include "native.h"

int64_t opcode_pack(uint8_t code, uint8_t A, uint16_t B, uint32_t C) {
	// C is max 20 bits
	if(C > 0x000fffff)
		error("C > 20 bits in opcode_pack()");

	return (int64_t)(code | (A<<8) | (B<<16) | (((int64_t)C)<<32));
}

void opcode_unpack(int64_t packed, uint8_t *code, uint8_t *A, uint16_t *B, uint32_t *C) {
	*code = ((uint64_t)packed) & 0xff;
	*A = (((uint64_t)packed) >> 8) & 0xff;
	*B = (((uint64_t)packed) >> 16) & 0xffff;
	*C = (((uint64_t)packed) >> 32) & 0x000fffff;
}

static char* opcode_names_by_index[] = {
	"FRAME-GET", "FRAME-SET", "JUMP-FORW", "JUMP-BACK", "CALL-BUILTIN", "CALL-USERWORD",
};

// only used during compilation so does not have to be fast
uint8_t opcode_name_to_code(const char *name) {
	for(int i=0; i<OPCODE_LAST_PLUS1; ++i) {
		if(!strcmp(opcode_names_by_index[i], name))
			return i;
	}

	error("No such opcode name: %s", name);	
}

const char* opcode_code_to_name(uint8_t code) {
	if(code >= OPCODE_LAST_PLUS1)
		error("No such opcode number: %d\n", code);

	return opcode_names_by_index[code];
}

// --- opcode functions ---

// A = number of frames up (0 means my frame); B = index of var in outer frame
void opcode_FRAME_GET(uint8_t levels, uint16_t index, uint32_t _unused) {
	if(!framedata)
		error("opcode FRAME-GET called on null frame");

	push(callframe_GetFrameObj(framedata, levels, index));
	
	// stats
	max_frame_slot_used = max(max_frame_slot_used,(int)index);
}

// A = number of frames up (0 means my frame); B = index of var in outer frame
void opcode_FRAME_SET(uint8_t levels, uint16_t index, uint32_t _unused) {
	if(!framedata)
		error("opcode FRAME-SET called on null frame");

	Object *obj = pop(); // get obj to be stored
	callframe_SetFrameObj(framedata, levels, index, obj);
	
	// stats
	max_frame_slot_used = max(max_frame_slot_used,(int)index);
}

void do_opcode_JUMP(int32_t offset) {
	//fprintf(stderr, "JUMP offset: %d\n", offset);
	set_codepos(get_codepos() + offset);
}

void opcode_JUMP_FORW(uint8_t _A, uint16_t _B, uint32_t offset) {
	do_opcode_JUMP(offset);
}

void opcode_JUMP_BACK(uint8_t _A, uint16_t _B, uint32_t offset) {
	do_opcode_JUMP(-((int32_t)offset));
}

void opcode_CALL_BUILTIN(uint8_t index, uint16_t _B, uint32_t _C) {
	call_builtin_by_index(index);
}

void opcode_CALL_USERWORD(uint8_t _A, uint16_t _B, uint32_t index) {
	call_userword_by_index(index);
}

// make sure order matches ordering of values!
opcode_func OPCODE_FUNCTIONS[] = {
	opcode_FRAME_GET,
	opcode_FRAME_SET,
	opcode_JUMP_FORW,
	opcode_JUMP_BACK,
	opcode_CALL_BUILTIN,
	opcode_CALL_USERWORD,
};
