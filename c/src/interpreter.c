
/*
	Interpreter

	Copyright (c) 2022 Frank McIngvale, see LICENSE
*/

#include "interpreter.h"
#include "errors.h"
#include "opcodes.h"
#include "native.h"
#include "langtypes.h"
#include "util.h"
#include "gc_object.h"
#include <stdio.h>

// sync with C++
#define STACK_SIZE (1<<16)
#define HEAP_STARTSIZE (1<<16)

#define MAX_CALLSTACK_DEPTH 1024

typedef struct _CallStackEntry {
	Object *code; // List holding code to run
	int pos;
	Object *framedata;
} CallStackEntry;

// stack of current and previous frames
CallStackEntry callstack[MAX_CALLSTACK_DEPTH];
int callstack_cur = -1; // index of currently running frame or -1

// shortcuts for current frame
Object *code; // List
int codepos;
Object *framedata;

// 2 memory areas: stack & free memory
//
// the stack is of fixed size, so it lives at the bottom of memory.
// program-allocated memory (vars) lives above it in a space that will be
// reallocated as needed
//
// plain integers are used to address all 3 areas
Object **OBJMEM = NULL;

// current stack pointer (points to item on top of stack), empty value and lowest usable index
int SP, SP_EMPTY, SP_MIN;
// starting index for program-allocatable memory
int HEAP_START;
// last valid index
int HEAP_END;
// next available index to allocate
int HEAP_NEXTFREE;

#if 0
int hasBuiltin(const char *name);

Object nextCodeObj();
Object nextCodeObjOrFail(const char *failmsg);
Object peekNextCodeObj();
Object prevCodeObj();
Object prevCodeObjOrFail(const char *failmsg);

Object nextSymbolOrFail(const char *failmsg);

// if the code is from a BoundLambda, pass it as the 3rd parameter
void code_call(ObjList *new_code, BoundLambda *bound_lambda=NULL);
int havePushedFrames();
void code_return();
#endif

// stats
void print_stats();
int max_callstack;
int min_run_SP;
unsigned long nr_tailcalls;
int max_frame_slot_used;
int nr_total_calls;
int nr_saved_frames;

typedef struct _WordDictEntry {
	const char *name;
	Object *list;
	UT_hash_handle hh;
} WordDictEntry;

Object *WORDS = NULL; // dictionary of user-defined words

void init_interpreter() {
	// stack at bottom, grows downward from SP_EMPTY
	// (the _EMPTY indexes are not valid storage locations, they indicate that the
	// respective stacks are empty)
	SP_MIN = 0;
	SP_EMPTY = SP_MIN + STACK_SIZE;
	SP = SP_EMPTY;

	// heap is next
	HEAP_START = SP_EMPTY;
	HEAP_END = HEAP_START + HEAP_STARTSIZE - 1;
	HEAP_NEXTFREE = HEAP_START;

	// clear userwords
	WORDS = newDict();

	// allocate stack+heap
	OBJMEM = (Object**)x_malloc((HEAP_END+1) * sizeof(Object*));

	// init callstack
	for(int i=0; i<MAX_CALLSTACK_DEPTH; ++i)
		callstack[i].framedata = new_CallFrameData();

	callstack_cur = -1;
	code = NULL;

	// init stats
	max_callstack = 0;
	min_run_SP = SP;
	nr_tailcalls = 0;
	max_frame_slot_used = 0;
	nr_total_calls = 0;
}

#if defined(USE_GC_OBJECT)
void interpreter_mark_reachable_objects() {
	// mark objects that can be found through my root objects

	// mark objects found in the stack
	if(OBJMEM) { // don't crash if not initted
		for(int i=SP; i<SP_EMPTY; ++i)
			gc_mark_object(OBJMEM[i]);

		// mark all objects found in heap
		for(int i=HEAP_START; i<HEAP_NEXTFREE; ++i)
			gc_mark_object(OBJMEM[i]);
	}

	// mark WORDS dict (which will then mark all code objects)
	if(WORDS)
		gc_mark_object(WORDS);

	// mark BUILTINS dict
	if(BUILTINS)
		gc_mark_object(BUILTINS);

	// mark objects in all active callframes
	for(int i=callstack_cur; i >= 0; --i) {
		gc_mark_object(callstack[i].code);
		gc_mark_object(callstack[i].framedata);		
	}

	// for INACTIVE call frames, need to mark the CallFrame object
	// to keep it alive (do NOT mark subobjects -- frame is inactive
	// so any objects in those slots can be freely deleted)
	if(OBJMEM) { // test to see if interpreter initted
		for(int i=callstack_cur+1; i < MAX_CALLSTACK_DEPTH; ++i)
			gc_mark_object_no_subobjects(callstack[i].framedata);
	}
}
#endif

void print_stats() {
	printf("\n==== Runtime Stats ====\n");
	printf("* General:\n");
	if(BUILTINS != NULL)
		printf("  Builtin words: %d\n", Dict_size(BUILTINS));
	else
		printf("  Builtin words: 0\n");
	printf("  User-defined words: %d\n", List_length(getWordlist()));
	printf("  Max stack depth: %d\n", (SP_EMPTY - min_run_SP));
	printf("  Max callstack depth: %d\n", max_callstack);
	printf("  Max callframe data slot: %d\n", max_frame_slot_used);
	printf("  Saved frames: %d\n", nr_saved_frames);
	printf("  Total calls: %d\n", nr_total_calls);
	printf("  Tail calls: %lu\n", nr_tailcalls);
	
	double tottime = current_system_cpu_time() - STARTUP_TIME;
	printf("  Total time: %lf\n", tottime);

	printf("* C:\n");
	x_mem_print_stats();
	langtypes_print_stats();
	print_gc_object_stats();

	printf("* Notices:\n");
	if(SP != SP_EMPTY) {
		printf("  Stack is not empty! (%d items)\n", (SP_EMPTY-SP));
		printf(" => %s\n", reprStack());
	}
}

void push(Object *obj) {
	if(SP <= SP_MIN)
		error("Stack overflow");
	
	OBJMEM[--SP] = obj;
	// stats
	min_run_SP = min(min_run_SP,SP);
}

Object* pop() {
	if(SP >= SP_EMPTY)
		error("Stack underflow");
	
	// set slot to null first so gc won't hold onto it (technically this is not necessary
	// for gc_object since it only counts the active stack items, but this will help
	// a traditional gc that scans all memory)
	Object *robj = OBJMEM[SP];
	OBJMEM[SP++] = newNull();
	return robj;
}

int get_SP() { return SP; }

void set_SP(int addr) {
	if(addr < SP_MIN || addr > SP_EMPTY)
		error("Bad address in set_sp: %d", addr);

	SP = addr;
	// stats
	min_run_SP = min(min_run_SP, SP);
}

int stack_depth() {
	return SP_EMPTY - SP;
}

int get_codepos() {
	return codepos;
}

void set_codepos(int pos) {
	if(!code)
		error("set_codepos() called but no code running");

	// NOTE: jump is allowed to go 1 past end of code -- this is a normal eof,
	// so the second test is > instead of >=
	if(pos < 0 || pos > List_length(code))
		error("Out of bounds in set_codepos()");

	codepos = pos;
}

const char* reprStack() {
	// like other functions that return temp strings, use a String object
	Object *str;
	for(int i=SP_EMPTY-1; i>=SP; --i) {
		string_printf(str, "%s ", fmtStackPrint(OBJMEM[i]));		
	}
	return string_cstr(str);
}

int heap_alloc(int nr) {
	if((HEAP_NEXTFREE + nr) >= HEAP_END) {
		// not enough memory, double it
		size_t newsize = max(HEAP_END+nr, (HEAP_END+1)*2);
		OBJMEM = (Object**)x_realloc(OBJMEM, newsize*sizeof(Object*));
		HEAP_END = newsize - 1;
	}
	int addr = HEAP_NEXTFREE;
	HEAP_NEXTFREE += nr;
	// init memory to nulls
	for(int i=0; i<nr; ++i) {
		OBJMEM[addr+i] = newNull();
	}
	return addr;
}

Object *heap_get(int addr) {
	if(addr < 0 || addr > HEAP_END)
		error("Bad address in heap_set: %d", addr);
		
	return OBJMEM[addr];
}

void heap_set(int addr, Object *obj) {
	if(addr < 0 || addr > HEAP_END)
		error("Bad address in heap_set: %d", addr);
		
	OBJMEM[addr] = obj;
}

Object* nextCodeObj() {
	if(!code || codepos >= List_length(code))
		return newVoid();

	return List_get(code, codepos++);
}

Object* nextCodeObjOrFail(const char *failmsg) {
	Object *o = nextCodeObj();
	if(isVoid(o))
		error("End of input: %s", failmsg);
	
	return o;
}

Object* peekNextCodeObj() {
	if(!code || codepos >= List_length(code))
		return newVoid();
	
	return List_get(code, codepos);
}

Object* prevCodeObj() {
	if(!code || codepos == 0)
		return newVoid();
	
	return List_get(code, --codepos);
}

Object* prevCodeObjOrFail(const char *failmsg) {
	Object *o = prevCodeObj();
	if(isVoid(o))
		error("No previous object: %s", failmsg);
	
	return o;
}

Object* lookupUserWord(const char *name) {
	return Dict_get(WORDS, name);
}

int haveUserWord(const char *name) {
	return isVoid(lookupUserWord(name)) ? 0 : 1;
}

Object* getWordlist(void) {
	Object *list = newList();
	for(ObjDictEntry *ent=WORDS->data.objdict; ent != NULL; ent=ent->hh.next) {
		//printf("ADD WORDLIST: %s\n", ent->name);
		List_append(list,newSymbol(ent->name,-1));
	}

	return list;
}

void defineWord(const char *name, Object *list, int allow_overwrite) {
	if(allow_overwrite || haveUserWord(name) == 0)
		Dict_put(WORDS, name, list);
	else
		error("Trying to redefine word: %s", name);
}

void deleteUserWord(const char* name) {
	if(!haveUserWord(name)) 
		error("Trying to delete non-existent word: %s", name);

	Dict_delete(WORDS, name);
}

// bound_outer is outer framedata or NULL if not a bound lambda
void code_call(Object *new_code_list, Object *bound_outer) {
	if(callstack_cur < 0)
		error("code_call but no code is running");
	else if(callstack_cur >= (MAX_CALLSTACK_DEPTH-1))
		error("Max callstack depth exceeded");

	//printf("CODE CALL: %s, OUTER: %s\n", fmtStackPrint(new_code_list), bound_outer? "YES":"NO");

	++nr_total_calls;

	// save current context -- only .pos needs to be updated
	callstack[callstack_cur].pos = codepos;

	// setup new frame
	++callstack_cur;
	callstack[callstack_cur].code = new_code_list;
	callstack[callstack_cur].pos = 0;
	// clear framedata to help GC
	callframe_clear(callstack[callstack_cur].framedata->data.framedata);
	
	// set shortcuts to new frame
	code = new_code_list;
	codepos = 0;
	framedata = callstack[callstack_cur].framedata;

	// when the bound lambda was created, the current frame (at the time) was saved
	// as its .outer frame. when the bound lambda runs here in a new frame, it needs
	// to have its .outer frame connected to the same .outer as when it was created,
	// so it has access to the saved data (closure) -- bound_outer can also be NULL
	// if no outer frame
	callframe_setOuter(framedata, bound_outer);		
	
	// stats
	max_callstack = max(max_callstack,(int)(callstack_cur+1));
}

int havePushedFrames() {
	return callstack_cur >= 0;
}

void code_return() {
	if(!callstack_cur < 0)
		error("code_return but no code is running");

	// did framedata become bound?
	if(callframe_isBound(callstack[callstack_cur].framedata)) {
		// framedata has been bound to one or more lambdas, so it cannot be 
		// freed. replace it in the stack with a new frame.
		callstack[callstack_cur].framedata = new_CallFrameData();
		// stats
		++nr_saved_frames;
	}

	// pop frame
	--callstack_cur;
	// set shortcuts, IF that wasn't the last frame (if it was, then these won't be used again)
	if(callstack_cur >= 0) {
		code = callstack[callstack_cur].code;
		codepos = callstack[callstack_cur].pos;
		framedata = callstack[callstack_cur].framedata;
	}
}

void run(Object *objlist) {
	if(callstack_cur >= 0)
		error("Interpreter run() called recursively");

	if(!objlist)
		error("Got NULL* as code in run()");

	if(!isList(objlist))
		error("Expecting list in run() but got: %s", fmtStackPrint(objlist));

	// almost but not quite code_call() so have to do it here
	callstack_cur = 0;
	callstack[callstack_cur].code = objlist;
	callstack[callstack_cur].pos = 0;
	// clear data to help GC
	callframe_clear(callstack[callstack_cur].framedata->data.framedata);

	// set shortcuts that are used everywhere else
	code = objlist;
	codepos = 0;
	framedata = callstack[callstack_cur].framedata;

	while(1) {
		#if defined(USE_GC_OBJECT)
		// use a higher number for production, but I like a lower value for testing
		// since it stress tests the GC more
		//if(GCOBJ_OBJECTS_SINCE_COLLECT > 500000) {
		if(GCOBJ_OBJECTS_SINCE_COLLECT > 10000000) {
			printf("RUNNING COLLECTION\n");
			gc_object_collect();
		}
		#endif

		Object *obj = nextCodeObj();
		
		//printf("STACK: [ %s ]\n", reprStack());
		//printf("RUN (pos=%d): %s\n", codepos-1, fmtStackPrint(obj));

		if(isOpcode(obj)) {
			uint8_t code, A;
			uint16_t B;
			uint32_t C;
			//fprintf(stderr, "OPCODE: %lx\n", obj->data.opcode);
			opcode_unpack(obj->data.opcode, &code, &A, &B, &C);
			if(code < 0 || code >= OPCODE_LAST_PLUS1)
				error("Bad opcode: %d", code);

			(*OPCODE_FUNCTIONS[code])(A, B, C);
			continue;
		}
		else if(isSymbol(obj)) {
			if(isSymbolMatch(obj, "'", 1)) {
				// quoted symbol - remove one level of quoting and push
				push(newSymbol(string_cstr(obj)+1, -1));
				continue;
			}
			
			if(isSymbolMatch(obj, "return", -1)) {
				// return from word by popping back to previous wordlist
				code_return();
				// if exited top level, exit program
				if(callstack_cur < 0)
					return;
				
				continue;
			}

			if(isSymbolMatch(obj, "if", -1)) {
				Object *cond = pop();
				//cout << "POPPED COND: " << cond.repr() << endl;
				if(!isBool(cond))
					error("'if' requires true|false but got: %s", fmtStackPrint(cond));
				
				// if TRUE, continue with the next instruction (so do nothing here)
				// if FALSE, skip the next instruction
				if(!cond->data.i)
					codepos += 1;
				
				continue;
			}

			if(isSymbolMatch(obj, "@", 1)) {
				// jump target -- ignore
				continue;
			}

			if(isSymbolMatch(obj, "call", -1)) {
				// top of stack must be a lambda
				Object *val = pop();
				if(isLambda(val) || isBoundLambda(val)) {
					// now this is just like calling a userword, below
					// TODO -- tail call elimination??
					//syntax->pushObjList(val.asLambda());

					// NOTE - this is for both the bound & unbound case
					code_call(val->data.lambda->list, val->data.lambda->outer);
				}
				//else if(val.isBoundLambda()) {
				//	// as above but pass bound lambda so its new call frame will be
				//	// connected the same outer frame that was captured with bind-lambda
				//	code_call(val.data.boundLambda->objlist, val.data.boundLambda);
				//}
				else
					error("call expects a lambda or bound-lambda, but got: %s", fmtStackPrint(val));
				
				continue;
			}

			// builtins, then userwords
			if(isSymbol(obj)) {
				// **NOTE** strings containing NULLs won't work here
				Object *bltin = Dict_get(BUILTINS, string_cstr(obj));
				if(isVoidFunctionPtr(bltin)) {
					(bltin->data.funcptr)();
					continue;
				}
				
				// **NOTE** strings containing NULLs won't work here
				Object *wordlist = lookupUserWord(string_cstr(obj));
				if(isList(wordlist)) {
					// tail call elimination -- if i'm at the end of this wordlist OR next word is 'return', then
					// i don't need to come back here, so pop my wordlist first to stop stack from growing
					#if 1 // can turn off to test without tail call elimination, if desired
					// FIXME - don't eliminate at toplevel since code_call expects a non-empty stack
					if((callstack_cur >= 1) && (isVoid(peekNextCodeObj()) || isSymbolMatch(peekNextCodeObj(),"return",-1))) {
						code_return();
						++nr_tailcalls;
					}
					#endif
					// execute word by pushing its objlist and continuing (words never have an outer frame)
					code_call(wordlist, NULL);
					continue;
				}
			}

			error("Unknown word: %s", fmtStackPrint(obj));
		}
		
		else if(isVoid(obj)) {
			// i could be returning from a word that had no 'return',
			// so pop words like i would if it were a return

			// note a subtle (unintended) side effect here:
			//		10 20 void 30 40 5 make-list call
			// execution will stop after 20 since void makes the interpreter think it has reached
			// the end of the list. avoiding this by making 'return' mandatory runs into trouble for
			// dynamically created lists that are called -- doesn't seem worth it to have to check every
			// list before calling that it ends with 'return' and modifying it if not.
			// bottom line -- storing void (in ANY container) is a bad idea and this is just an d
			// example of one consequence

			code_return();
			if(callstack_cur < 0)
				return; // popped top frame, return
			
			continue;
		}
		
		// if object was created from a list literal ( [ ... ] ), then it must be deepcopied
		// (see DESIGN-NOTES.md).
		else if(isList(obj)) {
			push(deepcopy(obj));
			continue;
		}

		// everything else gets pushed -- initially i was only pushing objects that could be
		// parsed from source. however, given the dynamic nature of verbii, every type of
		// of object can end up in runnable code ... for example, dictionaries are not 
		// parseable from source but:
		//	[ ] make-dict 1 make-list make-lambda 
		// ... now the dictionary object is in runnable code
		else {
			push(obj);
			continue;
		}
		
		//throw LangError(string("Unknown word ") + obj.fmtDisplay());
	}
}

