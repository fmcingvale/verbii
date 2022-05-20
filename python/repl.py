from __future__ import annotations
from errors import LangError
from langtypes import LangString, fmtStackPrint
"""
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ implementation
"""
from interpreter import Interpreter
import sys, re, os

INITLIB = "../lib/init.verb.b"
COMPILERLIB = "../lib/compiler.verb.b"
PATCHESLIB = "../lib/patches.verb"

def compile_and_load(intr, text, allow_overwrite):
	import native
	native.ALLOW_OVERWRITE_WORDS = allow_overwrite

	# normally do NOT want to catch errors here since it's better for them to be
	# caught in the normal flow. however sometimes a compiler bug happens where this
	# needs to be uncommented see what it did

	#try:
	# push code, compile and load into interpreter
	intr.push(LangString(text))
	code = intr.lookupWordOrFail('compile-and-load-string')
	intr.run(code)
	#except LangError as exc:
	#	errmsg = "*** " + exc.msg + " ***"
	#	print(errmsg)
	#	# always backtrace here since this is an error in the compiler not user code
	#	print_backtrace(intr)
	#	sys.exit(1) # treat as fatal
		
	native.ALLOW_OVERWRITE_WORDS = False # set back to default

def deserialize_and_run(intr, filename):
	from deserialize import deserialize_stream
	fileIn = open(filename, "r")
	deserialize_stream(intr, fileIn)
	code = intr.lookupWordOrFail("__main__")
	intr.run(code)
	# always delete __main__ after running, else it will prevent other code from loading
	intr.deleteWord("__main__")
	
def debug_hook(intr: Interpreter, word: str):
	print("=> " + intr.reprStack())
	print("Run: " + str(word))
	sys.stdout.write("press ENTER to continue ...")
	sys.stdout.flush()
	sys.stdin.readline()

def compile_and_run(intr, text, singlestep, allow_overwrite=False):
	# push code, compile and load into interpreter
	compile_and_load(intr, text, allow_overwrite)
	
	# run __main__
	code = intr.lookupWordOrFail('__main__')

	if singlestep:
		intr.run(code,debug_hook)
	else:
		intr.run(code)

	# as above, delete __main__ to avoid inadvertent reuse
	intr.deleteWord("__main__")

def new_interpreter(verbose=False):
	"convenience to start interpreter and optionally load init lib"
	intr = Interpreter()

	# load serialized versions of init.verb and compiler.verb (required
	# to bootstrap interpreter)
	deserialize_and_run(intr, INITLIB)
	deserialize_and_run(intr, COMPILERLIB)
	
	# now load & run patches, allowing them to overwrite existing words
	# (this is the only place this is allowed)
	buf = open(PATCHESLIB,'r').read()
	# if this is likely to take a bit, print a message
	if verbose and len(buf) > 1000: print("Patching ...")
	
	compile_and_run(intr, buf, False, True)

	return intr

# returns string on error, None on success	
def safe_compile_and_run(intr, text, singlestep, backtrace_on_error):
	try:
		compile_and_run(intr, text, singlestep)
	except LangError as exc:
		errmsg = "*** " + exc.msg + " ***"
		if backtrace_on_error:
			print_backtrace(intr)
		
		return errmsg

def repl(singlestep, showstats):
	"Run interactively"
	print("Verbii running on Python {0}.{1}.{2}".format(sys.version_info.major, sys.version_info.minor, sys.version_info.micro))

	intr = new_interpreter(True)

	while(True):
		sys.stdout.write(">> ")
		sys.stdout.flush()
		line = sys.stdin.readline().strip()
		if len(line) == 0:
			return # eof
		if line == "quit" or line == ",q":
			if showstats: intr.print_stats()
			return
		
		err = safe_compile_and_run(intr, line, singlestep, True)
		if err is not None:
			print(err)
			intr = new_interpreter() # restart on error
		else:
			print("=> " + intr.reprStack())

def run_test_mode(filename: str):
	"""read one line at a time from file and run, printing results and stack. 
	used for unit testing"""
	intr = new_interpreter()

	re_blankline = re.compile(r"""(^[ \t\r\n]*$)""")
	fileIn = open(filename,'r')
	while True:
		line = fileIn.readline()
		if line == "": return # end of file
		if re_blankline.match(line):
			continue # skip blank lines
		
		line = line.rstrip()
		print(">> " + line) 

		# don't want backtraces in -test mode; if an unknown error occurs, rerun
		# case without -test to see backtrace
		err = safe_compile_and_run(intr, line, False, False)
		if err is not None:
			print(err)
			intr = new_interpreter() # restart on error
		else:	
			print("=> " + intr.reprStack())
		
def backtrace_curframe(intr: Interpreter):
	trace = ""
	nr = 7; # number of words to print in each frame
	while nr > 0:
		w = intr.prevCodeObject()
		if(w == None):
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

def run_file(intr: Interpreter, filename: str, singlestep: bool, showstats: bool):
	# run file
	buf = open(filename,'r').read()

	err = safe_compile_and_run(intr, buf, singlestep, True)
	if err is not None:
		print(err)
	elif showstats:
		intr.print_stats()

if __name__ == '__main__':
	testmode = False
	filename = None
	singlestep = False
	showstats = False
	for i,arg in enumerate(sys.argv[1:]):
		#print("ARG:",arg)
		if arg == '-test':
			testmode = True
		elif arg == '-step':
			singlestep = True
		elif arg == '-stats':
			showstats = True
		elif arg == '--':
			# pass rest of args to script
			import native
			# this will be pushed directly, so make into list of string objects
			native.NATIVE_CMDLINE_ARGS = []
			for narg in sys.argv[i+2:]:
				native.NATIVE_CMDLINE_ARGS.append(LangString(narg))

			#print("CMDLINE ARGS TO SCRIPT:",sys.argv[i+2:])
			break
		elif filename is None and os.path.exists(arg):
			filename = arg
		else:
			print("Unknown option: " + arg)
			sys.exit(1)

	if filename is None:
		repl(singlestep, showstats)
	elif testmode is True:
		run_test_mode(filename)
	else:
		intr = new_interpreter()
		run_file(intr, filename, singlestep, showstats)
		