//
// Frontend - most of the heavy lifting is in verbii code.
//
// Copyright (c) 2022 Frank McIngvale, see LICENSE
//

#include "interpreter.h"
#include "errors.h"
#include "xmalloc.h"
#include "deserialize.h"
#include "native.h"
#include "gc_object.h"

void backtrace_curframe() {
	Object *trace = newString("",0);
	
	int nr = 7; // number of words to print in each frame
	while(nr--) {
		Object *o = prevCodeObj();
		if(isVoid(o)) {
			printf("%s\n", string_cstr(trace));
			return;
		}
		else {
			// ugh ... gross way to put string together, but at this point, wasting memory is irrelevant
			Object *part = newString("",0);
			string_printf(part, "%s %s", fmtStackPrint(o), string_cstr(trace));
			trace = part;
		}
	}
	printf("%s\n", string_cstr(trace));
}

void print_backtrace() {
	int i=0;
	while(1) {
		printf("FRAME %d: ", i++);
		backtrace_curframe();
		if(havePushedFrames()) {
			code_return();
		}
		else {
			return;
		}
	}
}

void deserialize_and_run(const char *filename) {
	FILE *fileIn = fopen(filename, "rb");
	//printf("DESERIALIZE: %s\n", filename.c_str());
	if(!fileIn)
		error("No such file: %s", filename);

	deserialize_stream(fileIn);
	// run __main__ to setup any vars
	Object *code = lookupUserWord("__main__");
	if(!code || isVoid(code))
		error("Unable to find __main__ after deserializing: %s", filename);

	// delete __main__ *BEFORE* running it, since the code I'm about to run may
	// want to define __main__ itself
	deleteUserWord("__main__");

	run(code);
}

#include <limits.h>
#include <stdlib.h>
#include "util.h"
#include <setjmp.h>

int main(int argc, char *argv[]) {
	// cpu-time will be relative to this
	STARTUP_TIME = current_system_cpu_time();

	while(1) {
		if(setjmp(ERROR_JMP_BUF) == 0) {	
			x_mem_init();

			init_gc_object();
			init_langtypes(); // builtins needs small ints to be allocated first
			init_builtins();
			
			int SHOW_RUN_STATS = FALSE;
			int DO_PROFILING = FALSE;
			char *BOOTFILE = NULL;
			char *RUN_BFILE = NULL;
			// collect args that should be passed on to boot.verb, filtering out mine
			Object *cmdline_args = newList();

			// catch only the flags that have to be implemented natively
			// pass the rest through as-is to boot.verb code
			int i=1;
			while(i<argc) {
				if(!strcmp(argv[i], "-stats")) {
					SHOW_RUN_STATS = TRUE;
				}
				else if(!strcmp(argv[i], "-libdir")) {
					if(i >= (argc-1)) {
						printf("Missing argument after -libdir\n");
						exit(1);
					}
					Object *name = newString("",0);
					string_printf(name, "%s", argv[i+1]);
					//printf("NAME: %s\n", name.c_str());
					if(string_cstr(name)[string_length(name)-1] != '/' && 
						string_cstr(name)[string_length(name)-1] != '\\') {
						printf("-libdir paths must end with / or \\, got: %s\n", string_cstr(name));
						exit(1);
					}
					string_printf(name, "boot.verb.b");
					if(file_exists(string_cstr(name))) {
						//printf("EXISTS: %s\n", name.c_str());
						BOOTFILE = x_strdup(string_cstr(name));
					}
					// *ALSO* pass to script args since boot needs to know the paths
					List_append(cmdline_args, newString(argv[i],-1));
					List_append(cmdline_args, newString(argv[i+1],-1));
					
					++i;
				}
				else if(!strcmp(argv[i], "-profile")) {
					DO_PROFILING = TRUE;
					SHOW_RUN_STATS = TRUE; // -profile implies -stats
				}
				else if(!strcmp(argv[i], "-runb")) {
					if(i >= (argc-1)) {
						printf("Missing argument after -runb\n");
						exit(1);
					}
					RUN_BFILE = x_strdup(argv[i+1]);
				}
				else {
					List_append(cmdline_args, newString(argv[i],-1));
				}
				++i;
			}
			if(!RUN_BFILE && !BOOTFILE) {
				printf("Cannot find boot.verb.b -- maybe you need to pass '-libdir PATH'?\n");
				exit(1);
			}
			//printf("** BOOTFILE: %s\n", BOOTFILE.c_str());

			init_interpreter();
			//intr->PROFILE_CALLS = DO_PROFILING;
				
			// boot.verb expects cmdline args on top of stack on entry
			push(cmdline_args);
			//printf("STACK BEFORE BOOT: %s\n", intr->reprStack().c_str());
			if(RUN_BFILE)
				deserialize_and_run(RUN_BFILE);
			else
				deserialize_and_run(BOOTFILE);
					
			if(SHOW_RUN_STATS)
				print_stats();

			if(RUN_BFILE) x_free(RUN_BFILE);
			if(BOOTFILE) x_free(BOOTFILE);
			// made it here == successful exit
			break;
		}				
		else {
			if(STACKTRACE_ON_EXCEPTION)
				print_backtrace();

			printf("%s\n", ERROR_MESSAGE);
			if(EXIT_ON_EXCEPTION)
				break;	

			// else, loop again which will reinit interpreter, etc.
		}
	}	
#if 0
		catch (LangError &err) {
			auto errstr = "*** " + string(err.what()) + " ***";
			if(STACKTRACE_ON_EXCEPTION)
				print_backtrace(intr);
				
			printf("%s\n", errstr.c_str());
			// see if boot.verb requested to exit on exception or run again
			if(EXIT_ON_EXCEPTION)
				exit(1);
		}
	}
#endif
#if defined(USE_GC_OBJECT)
	shutdown_gc_object();
	shutdown_interpreter();
	printf("  Final bytes allocated: %llu\n", XMEM_TOTAL_BYTES_ALLOCATED);
	printf("  Final bytes freed:     %llu\n", XMEM_TOTAL_BYTES_FREED);
	printf("  Bytes lost:            %llu\n", XMEM_TOTAL_BYTES_ALLOCATED - XMEM_TOTAL_BYTES_FREED);
	x_mem_print_trace();
#endif
}
