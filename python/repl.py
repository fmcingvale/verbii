from __future__ import annotations
from errors import LangError
"""
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ implementation
"""
from interpreter import Interpreter
import sys, re, os

INITLIB = "../lib/init.txt"

def new_interpreter(noinit: bool):
	"convenience to start interpreter and optionally load init lib"
	intr = Interpreter()
	
	if not noinit:
		# run initlib to load its words first
		intr.addText(open(INITLIB,'r').read())
		intr.run()
		# don't want initlib in the backtrace history, once it has successfully loaded
		intr.reader.clearAll()

	return intr

def repl(noinit: bool):
	"Run interactively"
	intr = new_interpreter(noinit)
	
	while(True):
		sys.stdout.write(">> ")
		sys.stdout.flush()
		line = sys.stdin.readline()
		if line == "quit":
			return
		
		intr.addText(line)

		try:
			intr.run()
			print("=> " + intr.reprStack())
		except LangError as exc:
			print("*** " + exc.msg + " ***")

def run_test_mode(filename: str, noinit: bool, status: dict):
	"""read one line at a time from file and run, printing results and stack. 
	used for unit testing"""
	intr = new_interpreter(noinit)

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
		intr.reader.clearAll() # clear any leftover text from previous line run
		intr.addText(line)
		intr.run()
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
	print("Run: " + word)
	sys.stdout.write("press ENTER to continue ...")
	sys.stdout.flush()
	sys.stdin.readline()

def run_file(intr: Interpreter, filename: str, singlestep: bool):
	# run file
	buf = open(filename,'r').read()
	intr.addText(buf)
	if singlestep:
		intr.run(debug_hook)
	else:
		intr.run()

if __name__ == '__main__':
	noinit = False
	testmode = False
	filename = None
	singlestep = False
	for arg in sys.argv[1:]:
		if arg == '-noinit':
			noinit = True
		elif arg == '-test':
			testmode = True
		elif arg == '-step':
			singlestep = True
		elif filename is None and os.path.exists(arg):
			filename = arg
		else:
			print("Unknown option: " + arg)
			sys.exit(1)

	if filename is None:
		repl(noinit)
	elif testmode is True:
		status = {'done': False, 'max-count': 0}
		while not status['done']:
			try:
				run_test_mode(filename, noinit, status)
			except LangError as exc:
				print("*** " + exc.msg + " ***")
				#print("MAX LINE:",status)
				status['max-count'] += 1
	else:
		intr = new_interpreter(noinit)

		try:
			run_file(intr, filename, singlestep)
		except LangError as err:
			print("*** " + err.msg + " ***")
			print_backtrace(intr)
	