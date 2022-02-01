from __future__ import annotations
"""
	Interpreter - runs code.

	There is no compilation step, not really even a parsing step -- the interpreter
	runs directly from the wordlists from the Reader. This makes the code smaller and
	makes e.g. forward declarations really easy since nothing is evaluated until it
	runs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ implementation.
"""
import re
from errors import LangError
from reader import Reader

class Interpreter(object):
	RAM_SIZE = (1<<24)
	STACK_SIZE = (1<<16)
	LOCALS_SIZE = (1<<10)

	def __init__(self):
		self.reader = Reader()
		# 3 memory areas: stack, locals, free memory
		self.RAM = [0] * self.RAM_SIZE
		# RAM indexes: stack pointer, empty value and lowest usable index
		# stack starts at top of memory and grows downward
		self.SP_EMPTY = Interpreter.RAM_SIZE - 1
		self.SP = self.SP_EMPTY
		self.SP_MIN = self.SP_EMPTY - self.STACK_SIZE
		# same for locals
		self.LP_EMPTY = self.SP_MIN
		self.LP = self.LP_EMPTY
		self.LP_MIN = self.LP_EMPTY - self.LOCALS_SIZE
		# next memory address available and last usable index
		self.MEM_LAST = self.LP_MIN - 1
		self.MEM_NEXT = 0

		self.re_integer = re.compile(r"""(^[+\-]?[0-9]+$)""")
		
	def addText(self, text):
		self.reader.addText(text)

	def push(self, obj):
		# unlike in the C++ implementation, I can just push regular python objects
		# here ... no need for tagging
		if self.SP <= self.SP_MIN:
			raise LangError("Stack overflow")

		self.SP -= 1
		self.RAM[self.SP] = obj

	def pop(self):
		if self.SP >= self.SP_EMPTY:
			raise LangError("Stack underflow")

		obj = self.RAM[self.SP]
		self.SP += 1

	def reprStack(self) -> str:
		# TODO -- fixme to print objects like in C++ version
		s = ""
		i = self.SP_EMPTY-1
		while i >= self.SP:
			s += str(self.RAM[i]) + ' '
			i -= 1

		return s

	"""to avoid a lot of 'if word is NOne' checks, these require a non-empty
	word or they throw an exception ... for use in cases where there MUST
	be a next/previous word, or its a syntax error"""
	def nextWordOrFail(self) -> str:
		"get next word, which must be non-None, or raise an exception"
		word = self.reader.nextWord()
		if word is None:
			raise LangError("Unexpected end of input")
	
		return word

	def prevWordOrFail(self) -> str:
		"read previous word, which must be non-None, or raise exception"
		word = self.reader.prevWord()
		if word is None:
			raise LangError("Unable to find previous word")
	
		return word
		
	def run(self) -> None:
		# run one word at a time in a loop, with the reader position as the continuation		
		while True:
			# see C++ notes on why certain words are here vs in native.py .. short story, it's pretty arbitrary

			word = self.reader.nextWord()
			if word is None:
				# i could be returning from a word that had no 'return',
				# so pop words like i would if it were a return
				if self.reader.hasPushedWords():
					self.reader.popWords()
					continue
				else:
					return

			if self.re_integer.match(word):
				self.push(int(word))
				continue

			raise LangError("Unknown word " + word)
			