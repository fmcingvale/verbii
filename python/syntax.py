from reader import Reader
from errors import LangError
from langtypes import LangLambda, LangString, MAX_INT_31, MIN_INT_31

class Syntax(object):
	def __init__(self):
		self.reader = Reader()

	# mirrored from the Reader interface. only Syntax is allowed to
	# rewrite the object lists (delete/insertPrevObject)

	def addText(self, text): self.reader.addText(text)
	def clearAll(self): self.reader.clearAll()
	def pushWords(self, objs): self.reader.pushWords(objs)
	def popWords(self): self.reader.popWords()
	def hasPushedWords(self): return self.reader.hasPushedWords()

	def isInteger(self, text):
		has_digits = False
		i = 0
		if text[i] in '+-':
			i += 1

		while i<len(text) and text[i] in '0123456789':
			i += 1
			has_digits = True

		if has_digits and i == len(text):
			return True
		else:
			return False

	"""to avoid a lot of 'if word is NOne' checks, these require a non-empty
	word or they throw an exception ... for use in cases where there MUST
	be a next/previous word, or its a syntax error"""
	def nextWordOrFail(self):
		"get next word, which must be non-None, or raise an exception"
		word = self.nextObj()
		if word is None:
			raise LangError("Unexpected end of input")
	
		return word

	def prevWordOrFail(self):
		"read previous word, which must be non-None, or raise exception"
		word = self.prevObj()
		if word is None:
			raise LangError("Unable to find previous word")
	
		return word

	def parse_lambda(self):
		"This is straight from the C++ version; see comments there. For brevity I omitted most of them here"
		# delete the { i just read (see below)
		self.reader.deletePrevWord()

		wordlist = []
		nesting = 1
		while True:
			word = self.nextObj()
			if word is None:
				raise LangError("Unexpected end of input inside { .. }")
			# delete the { ... } as I read it -- will replace it with a callable object
			self.reader.deletePrevWord()
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
				
				#print("MAKING LAMBDA:",wordlist)
				# create lambda from wordlist
				_lambda = LangLambda(wordlist)
				
				# replace { ... } in wordlist with lambda
				self.reader.insertPrevWord(_lambda)
				# the first time I see { ... }, I have to return the lambda.
				# every subsequent time, the object will be pushed by the wordlist i just modified
				return _lambda
			
			else:
				wordlist.append(word)
	
	def parse_quoted_printstring(self):
		s = ""
		while True:
			word = self.reader.nextWord() # *NOT* Syntax::nextObj() - don't want any processing, i.e.
									      # don't want numbers in middle of string to be converted to ints
			if word is None:
				raise LangError("Unexpected end of input inside .\"")
			elif word == "\"":
				# insert code to make string and print it
				self.reader.insertPrevWord(LangString(s))
				self.reader.insertPrevWord("puts")
				for i in range(2):
					self.reader.prevWord()

				# return first word and interpreter will continue & read the puts next
				return self.reader.nextWord()
			else:
				s += word + " "

	def parse_comment(self):
		#print("PARSE COMMENT:")
		nesting = 1
		while True:
			w = self.reader.nextWord() # like above, don't want any processing inside comment
			#print("WORD:",w)
			if w is None:
				raise LangError("Unexpected end of input inside comment")
			elif w == ")":
				nesting -= 1
				if nesting == 0:
					return self.nextObj() # return object after comment
			elif w == "(":
				nesting += 1
				
	def nextObj(self):
		obj = self.reader.nextWord()
		#print("SYNTAX:",obj)
		if type(obj) != str:
			return obj # only symbols can match syntax (at least right now)

		if self.isInteger(obj):
			i = int(obj)
			if i > MAX_INT_31 or i < MIN_INT_31:
				raise LangError("Integer overflow")

			self.reader.deletePrevWord()
			self.reader.insertPrevWord(i)
			return i
		elif obj[0] == '#':
			f = float(obj[1:])
			self.reader.deletePrevWord()
			self.reader.insertPrevWord(f)
			return f
		elif obj == "{":
			return self.parse_lambda()
		elif obj == ".\"":
			return self.parse_quoted_printstring()
		elif obj == "(":
			return self.parse_comment()
		else:
			return obj # not syntax so return as is

	def peekObj(self):
		# I need the above processing to occur on peeked objects,
		# so do it this way ....
		obj = self.nextObj()
		if obj is not None:
			self.reader.prevWord() # rewind
	
		return obj

	def prevObj(self): return self.reader.prevWord()
	def peekPrevObj(self): return self.reader.peekWord()
