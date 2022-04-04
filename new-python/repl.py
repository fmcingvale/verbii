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

def deserialize_and_run(intr, filename):
	from deserialize import deserialize_stream
	fileIn = open(filename, "r")
	deserialize_stream(intr, fileIn)
	code = intr.WORDS['__main__']
	intr.run(code)
	
def new_interpreter():
	"convenience to start interpreter and optionally load init lib"
	intr = Interpreter()

	# load serialized versions of init.verb and compiler.verb (required
	# to bootstrap interpreter)
	deserialize_and_run(intr, INITLIB)
	deserialize_and_run(intr, COMPILERLIB)
	
	# remove __main__ so I don't inadvertently run it again (i.e. if a later
	# byte-compile fails, I don't want this to remain)
	del intr.WORDS['__main__']

	return intr

def debug_hook(intr: Interpreter, word: str):
	print("=> " + intr.reprStack())
	print("Run: " + str(word))
	sys.stdout.write("press ENTER to continue ...")
	sys.stdout.flush()
	sys.stdin.readline()

def compile_and_run(intr, text, singlestep):
	# push code, compile and load into interpreter
	intr.push(LangString(text))
	code = intr.WORDS['compile-and-load-string']
	intr.run(code)
	
	# run __main__
	code = intr.WORDS['__main__']

	if singlestep:
		intr.run(code,debug_hook)
	else:
		intr.run(code)

	# as above, delete __main__ to avoid inadvertent reuse
	del intr.WORDS['__main__']

# returns string on error, None on success	
def safe_compile_and_run(intr, text, singlestep, backtrace_on_error):
	try:
		compile_and_run(intr, text, singlestep)
	except LangError as exc:
		errmsg = "*** " + exc.msg + " ***"
		if backtrace_on_error:
			print_backtrace(intr)
		
		return errmsg

def repl(singlestep):
	"Run interactively"
	print("Verbii running on Python {0}.{1}.{2}".format(sys.version_info.major, sys.version_info.minor, sys.version_info.micro))

	intr = new_interpreter()

	while(True):
		sys.stdout.write(">> ")
		sys.stdout.flush()
		line = sys.stdin.readline().strip()
		if len(line) == 0:
			return # eof
		if line == "quit" or line == ",q":
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
	runnable_lines = 0 # how many lines have I seen that are non-blank
	while (line := fileIn.readline()) != "":
		if re_blankline.match(line):
			continue # skip blank lines
		
		sys.stdout.write(">> " + line) # line has \n at end already

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

def run_file(intr: Interpreter, filename: str, singlestep: bool):
	# run file
	buf = open(filename,'r').read()

	err = safe_compile_and_run(intr, buf, singlestep, True)
	if err is not None:
		print(err)

if __name__ == '__main__':
	testmode = False
	filename = None
	singlestep = False
	for i,arg in enumerate(sys.argv[1:]):
		#print("ARG:",arg)
		if arg == '-test':
			testmode = True
		elif arg == '-step':
			singlestep = True
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
		repl(singlestep)
	elif testmode is True:
		run_test_mode(filename)
	else:
		intr = new_interpreter()
		run_file(intr, filename, singlestep)
		