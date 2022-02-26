"""
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ version
"""
from math import floor
from errors import LangError
from interpreter import Interpreter
from langtypes import LangLambda, MemArray, LangString, FLOAT_PRECISION, fmtDisplay, fmtStackPrint

# see notes in C++ implementation of this function.
# this returns (quotient,mod) instead of taking mod as a return param.
def int_divmod(a, b):
	if b == 0:
		raise LangError("Divide by zero")
	
	quot = int(floor(float(abs(a)) / float(abs(b))))
	
	samesign = True if (a < 0 and b < 0) or (a >= 0 and b >= 0) else False

	if samesign:
		mod = a - quot*b
		return quot,mod
	else:
		mod = a + quot*b
		return -quot,mod

def builtin_divmod(I: Interpreter, a, b):
	quot,mod = int_divmod(a,b)
	I.pushInt(mod)
	I.pushInt(quot)

def builtin_define_word(I: Interpreter):
	name = I.syntax.nextWordOrFail()
	words = []
	while True:
		w = I.syntax.nextWordOrFail()
		#print("DEFINE WORD:",w)
		if w == ';':
			I.WORDS[name] = words
			return
		else:
			words.append(w)

# ( obj addr -- ) - save obj to addr
def builtin_set(I: Interpreter, obj, addr):
	# see if its a stack/locals address or allocated memory
	if type(addr) == int:
		if addr < 0 or addr >= I.SIZE_STACKLOCALS:
			raise LangError("Bad address in set!: " + str(addr))
		
		I.STACKLOCALS[addr] = obj
	elif isinstance(addr, MemArray):
		if addr.offset < 0 or addr.offset >= len(addr.mem):
			raise LangError("Offset out of bounds in set!")

		addr.mem[addr.offset] = obj
	else:
		raise LangError("Bad address in set!: " + fmtStackPrint(addr))
	
# ( addr -- obj ) load obj from addr and push to stack
def builtin_ref(I: Interpreter, addr):
	# see if its a stack/locals address or allocated memory
	if type(addr) == int:
		if addr < 0 or addr >= I.SIZE_STACKLOCALS: 
			raise LangError("Bad address in ref: " + str(addr))
		
		I.push(I.STACKLOCALS[addr])
	elif isinstance(addr, MemArray):
		if addr.offset < 0 or addr.offset >= len(addr.mem):
			raise LangError("Offset out of bounds in ref")

		I.push(addr.mem[addr.offset])
	else:
		raise LangError("Bad address in ref: " + fmtStackPrint(addr))

# set stack pointer from addr on stack
def builtin_setsp(I: Interpreter, addr):
	if addr < I.SP_MIN or addr > I.SP_EMPTY:
		raise LangError("Bad address in SP!: " + str(addr))
	
	I.SP = addr

# set locals pointer from addr on stack
def builtin_setlp(I: Interpreter, addr):
	if addr < I.LP_MIN or addr > I.LP_EMPTY:
		raise LangError("Bad address in LP!: " + str(addr))
	
	I.LP = addr

# pop top of stack and push to locals
def builtin_tolocal(I: Interpreter):
	if I.LP <= I.LP_MIN:
		raise LangError("Locals overflow")
	
	I.LP -= 1
	I.STACKLOCALS[I.LP] = I.pop()

# pop top locals and push to stack
def builtin_fromlocal(I: Interpreter):
	if I.LP >= I.LP_EMPTY:
		raise LangError("Locals underflow")
	
	I.push(I.STACKLOCALS[I.LP])
	I.LP += 1

def builtin_showdef(I):
	name = I.syntax.nextWordOrFail()
	if name not in I.WORDS:
		print("No such word: " + name)
		return

	wordlist = I.WORDS[name]
	sys.stdout.write(name + ": ")
	for w in wordlist:
		sys.stdout.write(str(w) + " ")

	print(";")
	
def popIntOrFloat(I):
	obj = I.pop()
	if type(obj) == int or type(obj) == float:
		return float(obj)
	else:
		raise LangError("Expecting float or int but got: " + fmtStackPrint(obj))
		
def builtin_fadd(I):
	b = popIntOrFloat(I)
	a = popIntOrFloat(I)
	I.push(a+b)

def builtin_fsub(I):
	b = popIntOrFloat(I)
	a = popIntOrFloat(I)
	I.push(a-b)

def builtin_fmul(I):
	b = popIntOrFloat(I)
	a = popIntOrFloat(I)
	I.push(a*b)

def builtin_fdiv(I):
	b = popIntOrFloat(I)
	if b == 0:
		raise LangError("Floating point overflow")

	a = popIntOrFloat(I)
	I.push(a/b)

def builtin_add(I):
	b = I.pop()
	a = I.pop()

	if type(a) == int and type(b) == int:
		I.pushInt(a+b)
	elif isinstance(a,MemArray) and type(b) == int:
		# make & modify a clone, else dup'ing then adding would give
		# you two of the same object
		a_ = a.clone()
		a_.offset += b
		I.push(a_)
	# now with swapped args
	elif isinstance(b,MemArray) and type(a) == int:
		# make & modify a clone, else dup'ing then adding would give
		# you two of the same object
		b_ = b.clone()
		b_.offset += a
		I.push(b_)
	else:
		raise LangError("Don't know how to add '{0}' and '{1}'".format(a,b))

def builtin_fsetprec(I, a):
	global FLOAT_PRECISION 
	FLOAT_PRECISION = a

def builtin_gt(I):
	b = popIntOrFloat(I)
	a = popIntOrFloat(I)
	I.push(a>b)

def builtin_puts(I, obj):
	if not isinstance(obj, LangString):
		raise LangError("puts requires string but got: " + fmtStackPrint(obj))

	sys.stdout.write(obj.s)

import sys
# the interpreter pops & checks the argument types, making the code shorter here
BUILTINS = {
	'+': ([], builtin_add),
	'-': ([int,int], lambda I,a,b: I.pushInt(a-b)),
	'*': ([int,int], lambda I,a,b: I.pushInt(a*b)),
	'/mod': ([int,int], builtin_divmod),
	'f+': ([], builtin_fadd),
	'f-': ([], builtin_fsub),
	'f*': ([], builtin_fmul),
	'f/': ([], builtin_fdiv),
	'f.setprec': ([int], builtin_fsetprec),
	'==': ([], lambda I: I.push(popIntOrFloat(I)==popIntOrFloat(I))),
	'>': ([], builtin_gt),
	'.c': ([int], lambda I,a: sys.stdout.write(chr(a))),
	# object means any type
	# format TOS for stack display and push string
	'repr': ([object], lambda I,o: I.push(LangString(fmtStackPrint(o)))),
	# format TOS as display string and push
	'str': ([object], lambda I,o: I.push(LangString(fmtDisplay(o)))),
	# print string from TOS
	'puts': ([object], builtin_puts),
	'int?': ([object], lambda I,o: I.push(type(o) == int)),
	# [] for no args
	'depth': ([], lambda I: I.push(I.SP_EMPTY - I.SP)),
	':': ([], builtin_define_word),
	'def': ([], builtin_define_word),
	'ref': ([object], builtin_ref),
	'set!': ([object,object], builtin_set),
	'SP': ([], lambda I: I.push(I.SP)),
	'SP!': ([int], builtin_setsp),
	'LP': ([], lambda I: I.push(I.LP)),
	'LP!': ([int], builtin_setlp),
	'>L': ([], builtin_tolocal),
	'L>': ([], builtin_fromlocal),
	'.showdef': ([], builtin_showdef),
}

