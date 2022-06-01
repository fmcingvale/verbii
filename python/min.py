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

BOOTFILE = "../lib/boot.verb.b"

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
		if intr.havePushedFrames():
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
	import sys
	showstats = False
	import native
	# build list of args to pass to boot.verb
	native.NATIVE_CMDLINE_ARGS = []
	for i,arg in enumerate(sys.argv[1:]):
		#print("ARG:",arg)
		if arg == '-stats':
			showstats = True
		elif arg == '--':
			# anything after '--' is ignored
			import native
			# this will be pushed directly, so make into list of string objects
			for narg in sys.argv[i+2:]:
				native.NATIVE_CMDLINE_ARGS.append(LangString(narg))

			#print("CMDLINE ARGS TO SCRIPT:",sys.argv[i+2:])
			break
		else:
			# unknown - pass to script
			native.NATIVE_CMDLINE_ARGS.append(LangString(arg))

	intr = None
	while True:
		try:
			intr = Interpreter()
			deserialize_and_run(intr, BOOTFILE)
			break
		except LangError as exc:
			errmsg = "*** " + exc.msg + " ***"
			print_backtrace(intr)
			print(errmsg)
			if native.EXIT_ON_EXCEPTION:
				sys.exit(1)
				
		
	