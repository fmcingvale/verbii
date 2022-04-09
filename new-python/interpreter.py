from __future__ import annotations
from langtypes import LangLambda, LangString, fmtStackPrint, MAX_INT_31, MIN_INT_31
"""
	Interpreter - runs code deserialized from bytecode.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ implementation.
"""
import re
from errors import LangError

class Interpreter(object):
	STACK_SIZE = (1<<10)
	LOCALS_SIZE = (1<<10)
	HEAP_STARTSIZE = (1<<16) # grows as needed

	def __init__(self):
		# 3 memory areas here: stack, locals and heap (program allocatable memory)
		# these are object slots - can each hold any size object
		self.OBJMEM = [0] * (self.STACK_SIZE + self.LOCALS_SIZE + self.HEAP_STARTSIZE)
		# indexes: stack pointer, empty value and lowest usable index
		# stack is first:
		self.SP_MIN = 0
		self.SP_EMPTY = self.SP_MIN + self.STACK_SIZE
		self.SP = self.SP_EMPTY
		# locals are next:
		self.LP_MIN = self.SP_EMPTY
		self.LP_EMPTY = self.LP_MIN + self.LOCALS_SIZE
		self.LP = self.LP_EMPTY
		# next free heap index to allocate
		# note -- heap never shrinks even when vars are deleted. however, vars are intended to
		# be toplevel only, so this is not really a practical problem
		self.HEAP_NEXTFREE = self.LP_EMPTY

		self.re_integer = re.compile(r"""(^[+\-]?[0-9]+$)""")
		self.re_lambda = re.compile(r"""\$<lambda ([0-9]+)>""")
		
		# user-defined words
		self.WORDS = {}
		# variables
		self.VARS = {}

		self.code = None
		self.codepos = 0
		self.callstack = []

		# stats
		self.max_callstack = 0
		self.min_run_SP = self.SP
		self.min_run_LP = self.LP
		self.nr_tailcalls = 0

	def print_stats(self):
		from native import BUILTINS

		print("\n==== Runtime Stats ====")
		print("* General:")
		print("  Builtin words: {0}".format(len(BUILTINS)))
		print("  User-defined words: {0}".format(len(self.WORDS)))
		print("  Max stack depth: {0}".format(self.SP_EMPTY - self.min_run_SP))
		print("  Max locals depth: {0}".format(self.LP_EMPTY - self.min_run_LP))
		print("  Max callstack depth: {0}".format(self.max_callstack))
		print("  Tail calls: {0}".format(self.nr_tailcalls))

	def heap_alloc(self, nr):
		"alloc space for nr objects, returning starting index"
		addr = self.HEAP_NEXTFREE
		if (self.HEAP_NEXTFREE + nr) >= len(self.OBJMEM):
			# double heapsize when i run out of memory
			self.OBJMEM = self.OBJMEM + [0]*len(self.OBJMEM)

		self.HEAP_NEXTFREE += nr
		return addr

	def code_call(self, code):
		#print("CODE CALL (POS={0}): {1}".format(self.codepos, fmtStackPrint(code)))
		self.callstack.append((self.code,self.codepos))
		self.code = code
		self.codepos = 0
		# stats
		self.max_callstack = max(self.max_callstack,len(self.callstack))

	def havePushedFrames(self):
		return len(self.callstack) > 0

	def code_return(self):
		self.code,self.codepos = self.callstack.pop()
		#print("CODE RETURN (POS={0}): {1}".format(self.codepos, fmtStackPrint(self.code)))

	def push(self, obj):
		# unlike in the C++ implementation, I can just push regular python objects
		# here ... no need for tagging
		if self.SP <= self.SP_MIN:
			raise LangError("Stack overflow")

		self.SP -= 1
		self.OBJMEM[self.SP] = obj
		# stats
		self.min_run_SP = min(self.min_run_SP,self.SP)

	def pushInt(self, a):
		"like push but checks for valid integer range"
		if a > MAX_INT_31 or a < MIN_INT_31:
			raise LangError("Integer overflow")

		self.push(a)

	def pop(self):
		if self.SP >= self.SP_EMPTY:
			raise LangError("Stack underflow")

		obj = self.OBJMEM[self.SP]
		self.SP += 1
		return obj

	def reprStack(self) -> str:
		s = ""
		i = self.SP_EMPTY-1
		while i >= self.SP:
			s += fmtStackPrint(self.OBJMEM[i]) + ' '
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
		if self.code is None: raise LangError("nextCodeObj called while not running!")
		if self.codepos >= len(self.code): return None

		obj = self.code[self.codepos]
		self.codepos += 1
		return obj

	def nextCodeObjOrFail(self):
		obj = self.nextCodeObj()
		if obj is None:
			raise LangError("Unexpected end of input")

		return obj

	def peekNextCodeObj(self):
		if self.code is None: raise LangError("peekNextCodeObj called while not running!")
		if self.codepos >= len(self.code): return None

		return self.code[self.codepos]

	def prevCodeObject(self):
		if self.code is None: raise LangError("prevCodeObject called while not running!")
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

	def hasWord(self, name):
		return name in self.VARS or name in self.WORDS

	def deleteWord(self, name):
		if name in self.VARS:
			del self.VARS[name]
		elif name in self.WORDS:
			del self.WORDS[name]
		else:
			raise LangError("Trying to delete non-existent name: " + name)
		
	def run(self, objlist, stephook=None) -> None:
		#if len(self.callstack):
		if self.code is not None:
			raise LangError("Attempting to call Interpreter.run() recursively")

		#self.code_call(objlist)
		self.code = objlist
		self.codepos = 0

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
				if self.havePushedFrames():
					self.code_return()
					continue
				else:
					self.code = None # mark self as not running
					return

			#print("RUN OBJ:",word)
			#print(" => " + self.reprStack())
								
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
				if self.havePushedFrames():
					self.code_return()
				else:
					self.code = None # mark self as not running
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
				if self.hasWord(name):
					raise LangError("Trying to redefine name: " + name)
			
				# alloc memory for var
				self.VARS[name] = self.heap_alloc(count)
				continue
		
			if word == "del":
				name = self.nextCodeObjOrFail()
				self.deleteWord(name)
				continue
		
			if word == "call":
				# see if top of stack is Lambda or list
				obj = self.pop()
				if isinstance(obj,LangLambda):
					#print("CALLING LAMBDA:",obj.wordlist)
					# now this is just like calling a userword, below
					# TODO -- tail call elimination??
					self.code_call(obj.objlist)
				elif type(obj) == list:
					# call list like lambda
					self.code_call(obj)
				else:				
					raise LangError("call expects a lambda or list, but got: " + fmtStackPrint(obj))

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
				# tail call elimination
				if self.peekNextCodeObj() is None or self.peekNextCodeObj() == "return":
					# no need to come back here, so go ahead and pop my call frame before
					# calling word -- callstack will never grow on recursive tail-calls now
					if self.havePushedFrames():
						self.code_return()
						self.nr_tailcalls += 1 # stats

				# execute word by pushing its wordlist and continuing
				self.code_call(self.WORDS[word])
				continue
	
			if word in self.VARS:
				self.pushInt(self.VARS[word])
				continue

			raise LangError("Unknown word " + word)
			