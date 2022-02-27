from __future__ import annotations
from codecs import replace_errors
from langtypes import LangLambda, MemArray, LangString, fmtStackPrint, fmtDisplay, MAX_INT_31, MIN_INT_31
"""
	Interpreter - runs code deserialized from bytecode.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ implementation.
"""
import re
from errors import LangError

class Interpreter(object):
	STACK_SIZE = (1<<16)
	LOCALS_SIZE = (1<<10)

	def __init__(self):
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

		self.code = []
		self.codepos = 0
		self.callstack = []

	def code_call(self, code):
		print("CODE CALL (POS={0}): {1}".format(self.codepos, fmtStackPrint(code)))
		self.callstack.append((self.code,self.codepos))
		self.code = code
		self.codepos = 0

	def code_return(self):
		self.code,self.codepos = self.callstack.pop()
		print("CODE RETURN (POS={0}): {1}".format(self.codepos, fmtStackPrint(self.code)))

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
				word = self.nextCodeObjOrFail()
				if type(word) == str and word[1:] == jumpword[2:]:
					return # found word, stop
		elif jumpword[:2] == "<<":
			# backward jump
			while True:
				word = self.prevCodeObjectOrFail()
				if type(word) == str and word[1:] == jumpword[2:]:
					return # found word, stop
		else:
			raise LangError("Bad jumpword " + jumpword)

	def nextCodeObj(self):
		if self.codepos >= len(self.code):
			return None

		obj = self.code[self.codepos]
		self.codepos += 1
		return obj

	def nextCodeObjOrFail(self):
		obj = self.nextCodeObj()
		if obj is None:
			raise LangError("Unexpected end of input")

		return obj

	def peekNextCodeObj(self):
		if self.codepos >= len(self.code):
			return None

		return self.code[self.codepos]

	def prevCodeObject(self):
		if self.codepos == 0:
			return None
		else:
			self.codepos -= 1
			return self.code[self.codepos]

	def prevCodeObjectOrFail(self):
		obj = self.prevCodeObject()
		if obj is None:
			raise LangError("Unexpected end of input")

		return obj
		
	def run(self, objlist, stephook=None) -> None:
		if len(self.callstack):
			raise LangError("Attempting to call Interpreter.run() recursively")

		self.code_call(objlist)

		from native import BUILTINS
		# run one object at a time in a loop	
		while True:
			# see C++ notes on why certain words are here vs in native.py .. short story, it's pretty arbitrary

			word = self.nextCodeObj()
			if stephook is not None:
				stephook(self, word)

			if word is None:
				# i could be returning from a word that had no 'return',
				# so do return, if possible
				if len(self.callstack):
					self.code_return()
					continue
				else:
					return

			print("RUN OBJ:",word)
			print(" => " + self.reprStack())
								
			# literals that get pushed
			if type(word) == int or type(word) == float or isinstance(word,LangString) or \
				isinstance(word, LangLambda):
				self.push(word)
				continue

			# quoted symbols - remove one level of quoting and push symbol
			if word[0] == "'":
				self.push(word[1:])
				continue

			# string are symbols
			if word == "return":
				# return from word by popping back to previous wordlist (if not at toplevel)
				if len(self.callstack):
					self.code_return()
				else:
					return # top level return exits program
			
				continue
		
			if word == "if":
				# true jump is required
				true_jump = self.nextCodeObj()
				# false word is optional
				peeked = self.peekNextCodeObj()

				if type(peeked) == str and (peeked[:2] == "<<" or peeked[:2] == ">>"):
					false_jump = self.nextCodeObj()
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
				name = self.nextCodeObjOrFail()
				count = self.nextCodeObjOrFail()
				if type(count) != int:
					raise LangError("Expecting int after var but got: " + fmtStackPrint(count))
				# must be unique userword
				if name in self.VARS:
					raise LangError("Trying to redefine variable " + name)
			
				# create MemArray and store in VARS
				self.VARS[name] = MemArray(count, 0)
				continue
		
			if word == "del":
				name = self.nextCodeObjOrFail()
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
				self.code_call(obj.wordlist)
				continue
		
			# builtins, then userwords, then vars

			if word in BUILTINS:
				argtypes,func = BUILTINS[word]
				if (self.SP + len(argtypes)) > self.SP_EMPTY:
					raise LangError("Stack underflow")

				args = []
				#print("CALL WORD:",word)
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
				self.code_call(self.WORDS[word])
				continue
	
			if word in self.VARS:
				self.push(self.VARS[word])
				continue

			raise LangError("Unknown word " + word)
			