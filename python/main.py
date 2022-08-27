#
# Experimental minimized frontend that ONLY loads & runs lib/boot.verb.b
# - modeled after c++ implementation
#
# Copyright (c) 2022 Frank McIngvale, see LICENSE
#

from __future__ import annotations
from errors import LangError
from langtypes import LangString, fmtStackPrint, isVoid
from interpreter import Interpreter

def backtrace_curframe(intr: Interpreter):
	trace = ""
	nr = 7; # number of words to print in each frame
	while nr > 0:
		w = intr.prevCodeObject()
		if isVoid(w):
			print(trace)
			return
		else:
			trace = fmtStackPrint(w) + ' ' + trace
		
		nr -= 1
	
	print(trace)

def print_backtrace(intr: Interpreter):
	i=0
	while True:
		sys.stdout.write("FRAME " + str(i) + ": ")
		i += 1
		backtrace_curframe(intr)
		if intr.can_pop_callframe():
			intr.code_return() # pop frame and print next
		else:
			return # end of callstack

def deserialize_and_run(intr, filename):
	from deserialize import deserialize_stream
	fileIn = open(filename, "r")
	deserialize_stream(intr, fileIn)
	code = intr.lookupWordOrFail("__main__")
	# like c++ implementation, delete __main__ *before* running it so 
	# the code i'm calling can redefine it
	intr.deleteWord("__main__")
	# run boot, doesn't return until program complete or exception raised
	intr.run(code)
	
if __name__ == '__main__':
	import sys, os
	showstats = False
	BOOTFILE = ""
	import native
	# build list of args to pass to boot.verb
	cmdline_args = []
	i = 1
	while i < len(sys.argv):
		arg = sys.argv[i]
		#print("ARG:",arg)
		if arg == '-stats':
			showstats = True
		elif arg == '-libdir':
			if (i+1) >= len(sys.argv):
				print("Missing path after -libdir")
				sys.exit(1)

			name = sys.argv[i+1]
			if name[-1] not in "\\/":
				print("Paths passed with -libdir must end in \\ or /")
				sys.exit(1)

			name += "boot.verb.b"
			if os.path.isfile(name):
				BOOTFILE = name

			# pass all '-libdir path' args to scripts since they need to have them as well
			cmdline_args.append(LangString(arg))
			cmdline_args.append(LangString(sys.argv[i+1]))

			i += 1
		elif arg == '--':
			# once I find '--', pass everything else (including '--') to boot.verb
			# this will be pushed directly, so make into list of string objects
			for narg in sys.argv[i:]:
				cmdline_args.append(LangString(narg))

			#print("CMDLINE ARGS TO SCRIPT:",sys.argv[i+2:])
			break
		else:
			# unknown - pass to script
			cmdline_args.append(LangString(arg))

		i += 1

	if len(BOOTFILE) == 0:
		print("Unable to find boot.verb.b - maybe you need to use '-libdir path' or set VERBII_BOOT?")
		sys.exit(1)

	intr = None
	while True:
		try:
			intr = Interpreter()
			# boot expects cmdline args on top of stack
			intr.push(cmdline_args)
			deserialize_and_run(intr, BOOTFILE)
			break
		except LangError as exc:
			errmsg = "*** " + exc.msg + " ***"
			if native.STACKTRACE_ON_EXCEPTION:
				print_backtrace(intr)
				
			print(errmsg)
			if native.EXIT_ON_EXCEPTION:
				sys.exit(1)

	if showstats:
		intr.print_stats()
				
		
	