"""
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ version
"""
from math import floor
from errors import LangError
from interpreter import Interpreter
from langtypes import MemArray, LangString

FLOAT_PRECISION = 17

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

def builtin_comment(I: Interpreter):
	while True:
		w = I.nextWordOrFail()
		if w == ")":
			return

def builtin_print_string(I: Interpreter):
	while True:
		word = I.nextWordOrFail()
		if word == "\"":
			return
		else:
			sys.stdout.write(word + ' ')

def builtin_define_word(I: Interpreter):
	name = I.nextWordOrFail()
	words = []
	while True:
		w = I.nextWordOrFail()
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
		raise LangError("Bad address in set!: " + reprObject(addr))
	
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
		raise LangError("Bad address in ref: " + reprObject(addr))

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

def fmtDisplay(obj):
	"get obj as string for normal program output (like '.')"
	from interpreter import CallableWordlist
	if type(obj) is int:
		return str(obj)
	elif type(obj) is float:
		fmt = "{0:." + str(FLOAT_PRECISION) + "g}"
		return fmt.format(obj)
	elif obj is True:
		return "true"
	elif obj is False:
		return "false"
	elif isinstance(obj, CallableWordlist):
		return "<lambda>"
	elif isinstance(obj, MemArray):
		return "var:{0}:{1}".format(len(obj.mem), obj.offset)
	elif isinstance(obj, LangString):
		return obj.s
	elif type(obj) is str:
		# strings are symbols - not normally printed, so they get a ' to differentiate from strings
		return "'" + obj
	else:
		raise LangError("Don't know how to print object: " + str(obj))

def fmtStackPrint(obj):
	"get obj as verbose string for stack display"
	from interpreter import CallableWordlist
	if type(obj) is int:
		return str(obj)
	elif type(obj) is float:
		fmt = "{0:." + str(FLOAT_PRECISION) + "g}"
		return '#' + fmt.format(obj)
	elif obj is True:
		return "true"
	elif obj is False:
		return "false"
	elif isinstance(obj, CallableWordlist):
		return "<lambda>"
	elif isinstance(obj, MemArray):
		return "var:{0}:{1}".format(len(obj.mem), obj.offset)
	elif isinstance(obj, LangString):
		# in a stack display, strings get " ... "
		return '"' + obj.s + '"'
	elif type(obj) is str:
		# ... and symbols do not get ' here
		return obj
	else:
		raise LangError("Don't know how to print object: " + str(obj))

def builtin_showdef(I):
	name = I.nextWordOrFail()
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
		raise LangError("Expecting float or int but got: " + reprObject(obj))
		
def builtin_make_lambda(intr: Interpreter):
	"This is straight from the C++ version; see comments there. For brevity I omitted most of them here"
	from interpreter import CallableWordlist
	# delete the { i just read (see below)
	intr.reader.deletePrevWord()

	wordlist = []
	nesting = 1
	while True:
		word = intr.nextWordOrFail()
		# delete the { ... } as I read it -- will replace it with a callable object
		intr.reader.deletePrevWord()
		if word == "{":
			# if I find inner lambdas, just copy them for now and later when they are run, 
			# this same process will happen for them
			nesting += 1
			wordlist.append(word)
		
		elif word == "}":
			nesting -= 1
			if nesting > 0:
				wordlist.append(word)
				continue
			
			# create CallableWordlist from wordlist
			_callable = CallableWordlist(wordlist)
			intr.LAMBDAS.append(_callable)
			index = len(intr.LAMBDAS)-1

			# replace { ... } in wordlist with "$<lambda index>" so a subsequent 'call'
			# will find it (note it would be impossible for user code to insert this word
			# from source since it contains whitespace)
			intr.reader.insertPrevWord("$<lambda {0}>".format(index))
			# the first time I see { ... }, I have to push the CallableWordlist.
			# every subsequent time, the object will be pushed by the wordlist i just modified
			intr.push(_callable)
			return
		
		else:
			wordlist.append(word)

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
	'(': ([], builtin_comment),
	'."': ([], builtin_print_string),
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
	'{': ([], builtin_make_lambda),
}

