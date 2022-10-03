/*
	Interpreter

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#ifndef __interpreter_h__
#define __interpreter_h__

// interpreter state is global
#include "langtypes.h"

// setup interpreter
void init_interpreter();

void push(Object *obj);
Object* pop();

void run(Object *list);

int get_SP();
void set_SP(int sp);

int get_codepos();
void set_codepos(int pos);

const char* reprStack();

// current frame data or NULL
extern CallFrameData *framedata;

// stats
void print_stats();
extern int max_callstack;
extern int min_run_SP;
extern unsigned long nr_tailcalls;
extern int max_frame_slot_used;
extern int nr_total_calls;
extern int nr_saved_frames;

// allocate heap memory to store nr Objects - returns starting index
int heap_alloc(int nr);
Object *heap_get(int addr);
void heap_set(int addr, Object *obj);

// user-defined words
// **NOTE** names here cannot contain NULLs
//
// lookup user-defined word (as List) or void if not found
Object* lookupUserWord(const char *name);
int haveUserWord(const char *name);	
void defineWord(const char *name, Object *list, int allow_overwrite);
void deleteUserWord(const char* name);
Object* getWordlist(); // returns a List

#endif // __interpreter_h__
