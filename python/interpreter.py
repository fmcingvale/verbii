from __future__ import annotations
from langtypes import fmtStackPrint, MAX_VINT, MIN_VINT, \
			isLambda, isSymbol, \
			isList, isVoid, LangVoid, CallFrameData, isBoundLambda, isOpcode, \
			fmtDisplay
"""
	Interpreter - runs code deserialized from bytecode.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the C++ implementation.
"""
import re
from errors import LangError

class Interpreter(object):
	STACK_SIZE = (1<<16)
	HEAP_STARTSIZE = (1<<16) # grows as needed

	def __init__(self):
		# 2 memory areas here: stack and heap (program allocatable memory)
		# these are object slots - can each hold any size object
		self.OBJMEM = [0] * (self.STACK_SIZE + self.HEAP_STARTSIZE)
		# indexes: stack pointer, empty value and lowest usable index
		# stack is first:
		self.SP_MIN = 0
		self.SP_EMPTY = self.SP_MIN + self.STACK_SIZE
		self.SP = self.SP_EMPTY
		# next free heap index to allocate
		# note -- heap never shrinks even when vars are deleted. however, vars are intended to
		# be toplevel only, so this is not really a practical problem
		self.HEAP_NEXTFREE = self.SP_EMPTY

		self.re_integer = re.compile(r"""(^[+\-]?[0-9]+$)""")
		self.re_lambda = re.compile(r"""\$<lambda ([0-9]+)>""")
		
		# user-defined words (access through methods only)
		self._WORDS = {}

		self.code = None
		self.codepos = 0
		self.framedata = None # current CallFrameData
		self.callstack = []

		# stats
		self.max_callstack = 0
		self.min_run_SP = self.SP
		self.nr_tailcalls = 0

	def print_stats(self):
		from native import BUILTINS

		print("\n==== Runtime Stats ====")
		print("* General:")
		print("  Builtin words: {0}".format(len(BUILTINS)))
		print("  User-defined words: {0}".format(len(self._WORDS)))
		print("  Max stack depth: {0}".format(self.SP_EMPTY - self.min_run_SP))
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

	def code_call(self, code, bound_lambda=None):
		#print("CODE CALL (POS={0}): {1}".format(self.codepos, fmtStackPrint(code)))
		self.callstack.append((self.code,self.codepos,self.framedata))
		self.code = code
		self.codepos = 0
		self.framedata = CallFrameData()
		if bound_lambda:
			self.framedata.setOuterFrame(bound_lambda.outer)
		# stats
		self.max_callstack = max(self.max_callstack,len(self.callstack))

	def havePushedFrames(self):
		return len(self.callstack) > 0

	def code_return(self):
		self.code,self.codepos,self.framedata = self.callstack.pop()
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
		# nope -- removed overflow checks
		
		#if a > MAX_VINT or a < MIN_VINT:
		#	raise LangError("Integer overflow")

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
				word = self.nextCodeObj()
				if isVoid(word): raise LangError("No such jump: " + jumpword)
				elif isSymbol(word) and word[1:] == jumpword[2:]:
					return # found word, stop
		elif jumpword[:2] == "<<":
			# backward jump
			while True:
				word = self.prevCodeObject()
				if isVoid(word): raise LangError("No such jump: " + jumpword)
				elif isSymbol(word) and word[1:] == jumpword[2:]:
					return # found word, stop
		else:
			raise LangError("Bad jumpword " + jumpword)

	# note that void is returned on EOF/not found conditions to differentiate from a
	# null object

	def nextCodeObj(self):
		if self.code is None: raise LangError("nextCodeObj called while not running!")
		if self.codepos >= len(self.code): return LangVoid()

		obj = self.code[self.codepos]
		self.codepos += 1
		return obj

	def nextCodeObjOrFail(self):
		obj = self.nextCodeObj()
		if isVoid(obj):
			raise LangError("Unexpected end of input")

		return obj

	def peekNextCodeObj(self):
		if self.code is None: raise LangError("peekNextCodeObj called while not running!")
		if self.codepos >= len(self.code): return LangVoid()

		return self.code[self.codepos]

	def prevCodeObject(self):
		if self.code is None: raise LangError("prevCodeObject called while not running!")
		if self.codepos == 0:
			return LangVoid()
		else:
			self.codepos -= 1
			return self.code[self.codepos]

	def prevCodeObjectOrFail(self):
		obj = self.prevCodeObject()
		if isVoid(obj):
			raise LangError("Unexpected end of input")

		return obj

	def hasWord(self, name):
		return name in self._WORDS

	def defineWord(self, name, objlist, allow_overwrite):
		if self.hasWord(name) and not allow_overwrite:
			raise LangError("Trying to redefine name: " + name)

		self._WORDS[name] = objlist

	def lookupWord(self, name):
		return self._WORDS.get(name, None)

	def lookupWordOrFail(self, name):
		lst = self.lookupWord(name)
		if lst is None: raise LangError("No such word: " + name)
		else: return lst

	def deleteWord(self, name):
		if name in self._WORDS:
			del self._WORDS[name]
		else:
			raise LangError("Trying to delete non-existent name: " + name)
		
	def run(self, objlist, stephook=None) -> None:
		from langtypes import deepcopy
		#if len(self.callstack):
		if self.code is not None:
			raise LangError("Attempting to call Interpreter.run() recursively")

		#self.code_call(objlist)
		self.code = objlist
		self.codepos = 0
		self.framedata = None

		from native import BUILTINS
		# run one object at a time in a loop	
		while True:
			# see C++ notes on why certain words are here vs in native.py .. short story, it's pretty arbitrary

			word = self.nextCodeObj()
			if stephook is not None:
				stephook(self, word)

			# symbols are the most common object in code, so check those first
			if isSymbol(word):
				# quoted symbols - remove one level of quoting and push symbol
				if word[0] == "'":
					self.push(word[1:])
					continue

				# string are symbols
				elif word == "return":
					# return from word by popping back to previous wordlist (if not at toplevel)
					if self.havePushedFrames():
						self.code_return()
					else:
						self.code = None # mark self as not running
						return # top level return exits program
				
					continue
			
				elif word == "if":
					# true jump is required
					true_jump = self.nextCodeObj()
					
					cond = self.pop()
					if cond != True and cond != False:
						raise LangError("'if' expects true or false but got: " + str(cond))
				
					# this just repositions the reader
					if cond:
						self.do_jump(true_jump)
					
					# else, continue with next instruction
					continue

				elif word[:2] == ">>" or word[:2] == "<<":
					self.do_jump(word)
					continue

				elif word[0] == "@":
					# jump target -- ignore
					continue

				elif word == "call":
					# see if top of stack is Lambda or list
					obj = self.pop()
					if isLambda(obj):
						#print("CALLING LAMBDA:",obj.wordlist)
						# now this is just like calling a userword, below
						# TODO -- tail call elimination??
						self.code_call(obj.objlist)
					elif isBoundLambda(obj):
						# like above but sets bound lambda
						self.code_call(obj.objlist, obj)
					else:				
						raise LangError("call expects a lambda or bound_lambda but got: " + fmtStackPrint(obj))

					continue
			
				# builtins, then userwords

				elif word in BUILTINS:
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

				elif word in self._WORDS:
					# tail call elimination
					if isVoid(self.peekNextCodeObj()) or self.peekNextCodeObj() == "return":
						# no need to come back here, so go ahead and pop my call frame before
						# calling word -- callstack will never grow on recursive tail-calls now
						if self.havePushedFrames():
							self.code_return()
							self.nr_tailcalls += 1 # stats

					# execute word by pushing its wordlist and continuing
					self.code_call(self._WORDS[word])
					continue
				else:
					raise LangError("Unknown word " + fmtDisplay(word))

			elif isOpcode(word):
				from opcodes import OPCODE_FUNCTIONS
				if word.code < 0 or word.code >= len(OPCODE_FUNCTIONS):
					raise LangError("Bad opcode: " + str(word.code))

				OPCODE_FUNCTIONS[word.code](self, word.A, word.B, word.C)
				continue

			elif isVoid(word):
				# i could be returning from a word that had no 'return',
				# so do return, if possible
				if self.havePushedFrames():
					self.code_return()
					continue
				else:
					self.code = None # mark self as not running
					return

			# list literals are deepcopied (see DESIGN-NOTES.txt)
			elif isList(word):
				self.push(deepcopy(word))
				continue

			# see C++ notes -- everything else gets pushed
			else:
				self.push(word)
				continue

			# not reached
			#raise LangError("Unknown word " + word)
			