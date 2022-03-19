from __future__ import annotations
"""
	Bootstrap compiler. Only needed to get the Verbii compiler working.
	
	This is standalone, no dependenciens (other than Python libs).

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Adapted from the Verbii compiler implementation.

	Here, I only need to be able to compile the syntax used in init.verb or 
	compiler.verb. The Verbii compiler will handle anything more advanced.

	Other things are minimal here, like error reporting, since this is
	only ever meant to compile a small number of files.
"""
import sys, re

# minimal replacements for langtypes so this can be standalone
class LangLambda(object):
	def __init__(self, objlist):
		self.objlist = objlist

class LangString(object):
	def __init__(self, s):
		self.s = s

WORDS = [] # (name,objlist) -- keep in order in case it ever matters

READFILE_WORDS = []
READFILE_POS = 0

def readerNext():
	global READFILE_POS
	global READFILE_WORDS

	if READFILE_POS >= len(READFILE_WORDS):
		return None

	w = READFILE_WORDS[READFILE_POS]
	READFILE_POS += 1
	return w

re_int = re.compile(r'''^[+\-]?[0-9]+$''')

def syntaxNext():
	while True:
		w = readerNext()
		if w is None: return None
		elif w == '(': syntaxComment() # no return -- loop to get next obj
		elif w == '{': return syntaxLambda()
		elif w == ':': syntaxWordDef() # no return -- loop to get next obj
		elif w[0] == '"': return syntaxString(w)
		elif re_int.match(w): return int(w)
		# don't need floats
		else: return w # regular word

def syntaxComment():
	nesting = 1
	while True:
		w = readerNext() # NOT syntaxNext, don't want to interpret contents of comment
		if w is None:
			raise Exception("End of input inside comment")
		elif w == ')':
			nesting -= 1
			if nesting == 0:
				return
		elif w == '(':
			nesting += 1

def syntaxLambda():
	objlist = []
	while True:
		o = syntaxNext()
		if o is None:
			raise Exception("End of input inside { .. }")
		elif o == '}':
			return LangLambda(objlist)
		else:
			objlist.append(o)

def syntaxWordDef():
	objlist = []
	name = syntaxNext()
	if name is None:
		raise Exception("Missing name in word definition")

	while True:
		o = syntaxNext()
		if o is None:
			raise Exception("End of input inside : .. ;")
		elif o == ';':
			WORDS.append((name,objlist))
			return # no return value for worddef
		else:
			objlist.append(o)

def syntaxString(first):
	s = first[1:]
	while True:
		# test is done here to allow for a one word string
		if s[-1] == '"':
			s = s[:-1].replace('%','%37').replace(' ','%32').replace('\n','%10').replace('\r','%13')
			return LangString(s)

		o = readerNext() # NOT syntaxNext() - don't want to try and intepret objs inside string
		if o is None:
			raise Exception("End of input inside \" .. \"")
		else:
			s += ' ' + o
		
def byteCompile():
	objs = []
	while True:
		obj = syntaxNext()
		if obj is None:
			return objs

		objs.append(obj)

def serialize(obj):
	if obj is None: print("n")
	elif type(obj) is int: print("i {0}".format(obj))
	elif obj is True: print("b true")
	elif obj is False: print("b false")
	elif isinstance(obj,LangString): print("s {0}".format(obj.s))
	elif type(obj) is str: print("y {0}".format(obj))
	elif type(obj) is list:
		print("L {0}".format(len(obj)))
		for o in obj:
			serialize(o)
	elif isinstance(obj,LangLambda):
		print("F")
		serialize(obj.objlist)
	else:
		raise Exception("Cannot serialize: " + str(obj))

if len(sys.argv) < 2:
	print("Usage: compiler sourcefile")
	sys.exit(1)

READFILE_WORDS = open(sys.argv[1],'r').read().split()
READFILE_POS = 0

# the code that is NOT in a word definition goes into the last word, named __main__
objs = byteCompile()
WORDS.append(('__main__',objs))

# now everything is a word def, so serialize as a list
print("L {0}".format(len(WORDS)))
for name,objs in WORDS:
	print("W {0}".format(name))
	serialize(objs)
	


