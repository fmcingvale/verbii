from __future__ import annotations
from langtypes import LangLambda, MemArray, LangString, fmtStackPrint, fmtDisplay, MAX_INT_31, MIN_INT_31

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
from syntax import Syntax

class Interpreter(object):
	STACK_SIZE = (1<<16)
	LOCALS_SIZE = (1<<10)

	def __init__(self):
		self.syntax = Syntax()
		# stack & locals live here -- integer addresses are indexes into this.
		# variables live in allocated memory tied to a MemArray.
		self.SIZE_STACKLOCALS = self.STACK_SIZE + self.LOCALS_SIZE
		self.STACKLOCALS = [0] * self.SIZE_STACKLOCALS
		# indexes: stack pointer, empty value and lowest usable index
		# stack starts at top of memory and grows downward
		self.SP_EMPTY = self.SIZE_STACKLOCALS - 1
		self.SP = self.SP_EMPTY
		self.SP_MIN = self.SP_EMPTY - self.STACK_SIZE
		# same for locals
		self.LP_EMPTY = self.SP_MIN
		self.LP = self.LP_EMPTY
		self.LP_MIN = self.LP_EMPTY - self.LOCALS_SIZE
	
		self.re_integer = re.compile(r"""(^[+\-]?[0-9]+$)""")
		self.re_lambda = re.compile(r"""\$<lambda ([0-9]+)>""")
		
		# user-defined words
		self.WORDS = {}
		# anonymous functions - referred to by $<lambda index> in modified wordlists
		self.LAMBDAS = []
		# variables
		self.VARS = {}

	def addText(self, text):
		self.syntax.addText(text)

	def push(self, obj):
		# unlike in the C++ implementation, I can just push regular python objects
		# here ... no need for tagging
		if self.SP <= self.SP_MIN:
			raise LangError("Stack overflow")

		self.SP -= 1
		self.STACKLOCALS[self.SP] = obj

	def pushInt(self, a):
		"like push but checks for valid integer range"
		if a > MAX_INT_31 or a < MIN_INT_31:
			raise LangError("Integer overflow")

		self.push(a)

	def pop(self):
		if self.SP >= self.SP_EMPTY:
			raise LangError("Stack underflow")

		obj = self.STACKLOCALS[self.SP]
		self.SP += 1
		return obj

	def reprStack(self) -> str:
		s = ""
		i = self.SP_EMPTY-1
		while i >= self.SP:
			s += fmtStackPrint(self.STACKLOCALS[i]) + ' '
			i -= 1

		return s
		
	def do_jump(self, jumpword: str):
		"take word like '>>NAME' or '<<NAME' and jump to '@NAME'"
		if jumpword[:2] == ">>":
			# forward jump, find word (>>NAME -> @NAME)
			while True:
				word = self.syntax.nextWordOrFail()
				if type(word) == str and word[1:] == jumpword[2:]:
					return # found word, stop
		elif jumpword[:2] == "<<":
			# backward jump
			while True:
				word = self.syntax.prevWordOrFail()
				if type(word) == str and word[1:] == jumpword[2:]:
					return # found word, stop
		else:
			raise LangError("Bad jumpword " + jumpword)

	def run(self, stephook=None) -> None:
		from native import BUILTINS
		# run one word at a time in a loop, with the reader position as the continuation		
		while True:
			# see C++ notes on why certain words are here vs in native.py .. short story, it's pretty arbitrary

			word = self.syntax.nextObj()
			if stephook is not None:
				stephook(self, word)

			if word is None:
				# i could be returning from a word that had no 'return',
				# so pop words like i would if it were a return
				if self.syntax.hasPushedWords():
					self.syntax.popWords()
					continue
				else:
					return

			# literals that get pushed
			if type(word) == int or type(word) == float or isinstance(word,LangString) or \
				isinstance(word, LangLambda):
				self.push(word)
				continue

			# string are symbols
			if word == "return":
				# return from word by popping back to previous wordlist (if not at toplevel)
				if self.syntax.hasPushedWords():
					self.syntax.popWords()
				else:
					return # top level return exits program
			
				continue
		
			if word == "if":
				# true jump is required
				true_jump = self.syntax.nextObj()
				# false word is optional
				peeked = self.syntax.peekObj()

				if type(peeked) == str and (peeked[:2] == "<<" or peeked[:2] == ">>"):
					false_jump = self.syntax.nextObj()
				else:
					false_jump = None
			
				cond = self.pop()
				if cond != True and cond != False:
					raise LangError("'if' expects true or false but got: " + str(cond))
			
				# these don't run the jump, they just reposition the reader
				if cond:
					self.do_jump(true_jump)
				elif false_jump is not None:
					self.do_jump(false_jump)
			
				continue

			if word[:2] == ">>" or word[:2] == "<<":
				self.do_jump(word)
				continue

			if word[0] == "@":
				# jump target -- ignore
				continue

			if word == "var":
				name = self.syntax.nextWordOrFail()
				count = self.syntax.nextObj()
				if type(count) != int:
					raise LangError("Expecting int after var but got: " + fmtStackPrint(count))
				# must be unique userword
				if name in self.VARS:
					raise LangError("Trying to redefine variable " + name)
			
				# create MemArray and store in VARS
				self.VARS[name] = MemArray(count, 0)
				continue
		
			if word == "del":
				name = self.syntax.nextWordOrFail()
				if name not in self.VARS:
					raise LangError("Trying to delete non-existent variable " + name)
				
				del self.VARS[name]
				continue
		
			if word == "call":
				# top of stack must be a CallableWordlist
				obj = self.pop()
				if not isinstance(obj,LangLambda):
					raise LangError("call expects a lambda, but got: " + fmtStackPrint(obj))

				#print("CALLING LAMBDA:",obj.wordlist)
				# now this is just like calling a userword, below
				# TODO -- tail call elimination??
				self.syntax.pushWords(obj.wordlist)
				continue
		
			# builtins, then userwords, then vars

			if word in BUILTINS:
				argtypes,func = BUILTINS[word]
				if (self.SP + len(argtypes)) > self.SP_EMPTY:
					raise LangError("Stack underflow")

				args = []
				#print("WORD:",word)
				#print("ARGTYPES:",argtypes)
				for t in reversed(argtypes):
					v = self.pop()
					#print("POPPED",v,t)
					if type(v) != t and t != object:
						raise LangError("Expecting type " + str(t) + " but got type " + str(type(v)) + " ({0}) for word {1}".format(v,word))

					args.insert(0,v)

				# func gets Interpreter as first arg
				args.insert(0,self)
				func(*args)
				continue

			if word in self.WORDS:
				# TODO -- tail call elimination
			
				# execute word by pushing its wordlist and continuing
				self.syntax.pushWords(self.WORDS[word])
				continue
	
			if word in self.VARS:
				self.push(self.VARS[word])
				continue

			raise LangError("Unknown word " + word)
			