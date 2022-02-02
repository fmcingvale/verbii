from __future__ import annotations
from errors import LangError
"""
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ implementation
"""
from interpreter import Interpreter
import sys, re, os
def repl():
	"Run interactively"
	intr = Interpreter()
	
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
			
def run_test_mode(filename: str, noinit: bool, maxlinerun: int):
	intr = Interpreter()

	if not noinit:
		# run initlib to load its words first
		intr.addText(open("initlib.txt",'r').read())
		intr.run()
		# don't want initlib in the backtrace history, once it has successfully loaded
		intr.reader.clearAll()
	
	re_blankline = re.compile(r"""(^[ \t\r\n]*$)""")
	fileIn = open(filename,'r')
	runnable_lines = 0 # how many lines have I seen that are non-blank
	while (line := fileIn.readline()) != "":
		if re_blankline.match(line):
			continue # don't count blank lines

		runnable_lines += 1

		if runnable_lines <= maxlinerun:
			# counts as running, since if i fail i want to restart at the NEXT line
			maxlinerun = max(maxlinerun,runnable_lines)
			continue
		
		# as above, update line before i run it
		maxlinerun = runnable_lines

		sys.stdout.write(">> " + line) # line has \n at end already
		intr.addText(line)
		intr.run()
		print("=> " + intr.reprStack())

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
		repl()
	elif testmode is True:
		try:
			run_test_mode(filename, noinit, 0)
		except LangError as exc:
			print("*** " + exc.msg + " ***")
			# TODO - need to restart after error
	else:
		print("Not implemented yet")
