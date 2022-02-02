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
		intr.addText(line)
		intr.run()
		print("=> " + intr.reprStack())
		# update count AFTER above suceeds
		status['max-count'] = runnable_lines
		#print(status['max-count'])

	# made it all the way through, set 'done'
	status['done'] = True

if __name__ == '__main__':
	noinit = False
	testmode = False
	filename = None
	for arg in sys.argv[1:]:
		if arg == '-noinit':
			noinit = True
		elif arg == '-test':
			testmode = True
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
		print("Not implemented yet")
