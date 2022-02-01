from __future__ import annotations
"""
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ implementation
"""
from interpreter import Interpreter
import sys
def repl():
	intr = Interpreter()
	
	while(True):
		sys.stdout.write(">> ")
		sys.stdout.flush()
		line = sys.stdin.readline()
		if line == "quit":
			return
		
		intr.addText(line)

		intr.run()
		print("=> " + intr.reprStack())
		
if __name__ == '__main__':
	repl()
	