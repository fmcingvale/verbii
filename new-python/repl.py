from __future__ import annotations
from errors import LangError
from langtypes import LangString
"""
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ implementation
"""
from interpreter import Interpreter
import sys, re, os

INITLIB = "../lib/init.verb.b"
COMPILERLIB = "../lib/compiler.verb.b"

def new_interpreter():
	"convenience to start interpreter and optionally load init lib"
	intr = Interpreter()

	# load serialized versions of init.verb and compiler.verb (required
	# to bootstrap interpreter)
	from deserialize import deserialize_stream
	fileIn = open(INITLIB,"r")
	deserialize_stream(intr, fileIn)
	code = intr.WORDS['__main__']
	intr.run(code)
	fileIn = open(COMPILERLIB,"r")
	deserialize_stream(intr, fileIn)

	return intr

def repl():
	"Run interactively"
	
	intr = new_interpreter()

	while(True):
		sys.stdout.write(">> ")
		sys.stdout.flush()
		line = sys.stdin.readline()
		if len(line) == 0:
			return # eof
		if line == "quit":
			return
		
		#intr.addText(line)
		intr.push(LangString(line))
		code = intr.WORDS['byte-compile-string']
		#intr.run(code)
		try:
			intr.run(code)
		except LangError as exc:
			print("*** " + exc.msg + " ***")
			continue

		#print("COMPILED OK")
		# byte-compile leaves list of words on stack -- used by serializer -- but i
		# don't need them here
		intr.pop()
		
		# run __main__
		code = intr.WORDS['__main__']
		try:
			intr.run(code)
			print("=> " + intr.reprStack())
		except LangError as exc:
			print("*** " + exc.msg + " ***")

def run_test_mode(filename: str, status: dict):
	"""read one line at a time from file and run, printing results and stack. 
	used for unit testing"""
	intr = new_interpreter()

	re_blankline = re.compile(r"""(^[ \t\r\n]*$)""")
	fileIn = open(filename,'r')
	runnable_lines = 0 # how many lines have I seen that are non-blank
	while (line := fileIn.readline()) != "":
		if re_blankline.match(line):
			continue # don't count blank lines

		runnable_lines += 1
		#print("LINE:",runnable_lines)

		if runnable_lines <= status['max-count']:
			# skipping line counts as running since either i successfully ran
			# it previously, or am skipping it because it crashed
			status['max-count'] = max(status['max-count'],runnable_lines)
			continue
		
		sys.stdout.write(">> " + line) # line has \n at end already

		# compile and run line, like above
		intr.push(LangString(line))
		code = intr.WORDS['byte-compile-string']
		intr.run(code)
		intr.pop() # don't need wordlist
		
		code = intr.WORDS['__main__']
		intr.run(code)

		print("=> " + intr.reprStack())
		# update count AFTER above suceeds
		status['max-count'] = runnable_lines
		#print(status['max-count'])

	# made it all the way through, set 'done'
	status['done'] = True

def backtrace_curframe(intr: Interpreter):
	trace = ""
	nr = 7; # number of words to print in each frame
	while nr > 0:
		w = intr.reader.prevWord()
		if(w == None):
			print(trace)
			return
		else:
			trace = w + ' ' + trace
		
		nr -= 1
	
	print(trace)

def print_backtrace(intr: Interpreter):
	i=0
	while True:
		sys.stdout.write("FRAME " + str(i) + ": ")
		i += 1
		backtrace_curframe(intr)
		if intr.reader.hasPushedWords():
			intr.reader.popWords()
		else:
			return

def debug_hook(intr: Interpreter, word: str):
	print("=> " + intr.reprStack())
	print("Run: " + str(word))
	sys.stdout.write("press ENTER to continue ...")
	sys.stdout.flush()
	sys.stdin.readline()

def run_file(intr: Interpreter, filename: str, singlestep: bool):
	# run file
	buf = open(filename,'r').read()

	intr.push(LangString(buf))
	code = intr.WORDS['byte-compile-string']
	#intr.run(code)
	try:
		intr.run(code)
	except LangError as exc:
		print("*** " + exc.msg + " ***")
		return

	# byte-compile leaves list of words on stack -- used by serializer -- but i
	# don't need them here
	intr.pop()
	
	#print("PRESS ENTER TO RUN ...")
	#sys.stdin.readline()

	# run __main__
	code = intr.WORDS['__main__']
	try:
		intr.run(code) #, debug_hook)
		#print("=> " + intr.reprStack())
	except LangError as exc:
		print("*** " + exc.msg + " ***")

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
		repl()
	elif testmode is True:
		status = {'done': False, 'max-count': 0}
		while not status['done']:
			try:
				run_test_mode(filename, status)
			except LangError as exc:
				print("*** " + exc.msg + " ***")
				#print("MAX LINE:",status)
				status['max-count'] += 1
	else:
		intr = new_interpreter()

		try:
			run_file(intr, filename, singlestep)
		except LangError as err:
			print("*** " + err.msg + " ***")
			print_backtrace(intr)
	