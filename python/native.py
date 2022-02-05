"""
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from C++ version
"""
from math import floor
from errors import LangError
from interpreter import Interpreter

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
	if addr < 0 or addr >= len(I.RAM):
		raise LangError("Bad address in set!: " + str(addr))
	
	I.RAM[addr] = obj

# ( addr -- obj ) load obj from addr and push to stack
def builtin_ref(I: Interpreter, addr):
	if addr < 0 or addr >= len(I.RAM): 
		raise LangError("Bad address in ref: " + str(addr))
	
	I.push(I.RAM[addr])

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
	I.RAM[I.LP] = I.pop()

# pop top locals and push to stack
def builtin_fromlocal(I: Interpreter):
	if I.LP >= I.LP_EMPTY:
		raise LangError("Locals underflow")
	
	I.push(I.RAM[I.LP])
	I.LP += 1

def reprObject(obj):
	from interpreter import CallableWordlist
	if type(obj) is int:
		return str(obj)
	elif obj is True:
		return "true"
	elif obj is False:
		return "false"
	elif isinstance(obj, CallableWordlist):
		return "<lambda>"
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

import sys
# the interpreter pops & checks the argument types, making the code shorter here
BUILTINS = {
	'+': ([int,int], lambda I,a,b: I.pushInt(a+b)),
	'-': ([int,int], lambda I,a,b: I.pushInt(a-b)),
	'*': ([int,int], lambda I,a,b: I.pushInt(a*b)),
	'/mod': ([int,int], builtin_divmod),
	'==': ([int,int], lambda I,a,b: I.push(a==b)),
	'>': ([int,int], lambda I,a,b: I.push(a>b)),
	'.c': ([int], lambda I,a: sys.stdout.write(chr(a))),
	# object means any type
	'repr': ([object], lambda I,o: sys.stdout.write(reprObject(o))),
	# [] for no args
	'depth': ([], lambda I: I.push(I.SP_EMPTY - I.SP)),
	'(': ([], builtin_comment),
	'."': ([], builtin_print_string),
	':': ([], builtin_define_word),
	'def': ([], builtin_define_word),
	'ref': ([int], builtin_ref),
	'set!': ([object,int], builtin_set),
	'SP': ([], lambda I: I.push(I.SP)),
	'SP!': ([int], builtin_setsp),
	'LP': ([], lambda I: I.push(I.LP)),
	'LP!': ([int], builtin_setlp),
	'>L': ([], builtin_tolocal),
	'L>': ([], builtin_fromlocal),
	'.showdef': ([], builtin_showdef),
	'{': ([], builtin_make_lambda),
}

