from __future__ import annotations
"""
	Reader - splits text into words and provides next/previous interface.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	This mirrors the C++ implentation, but hopefully more Pythonically.
"""
from errors import LangError
class Reader(object):
	def __init__(self):
		self.clearAll()

	def addText(self, text: str):
		"add more text to current context"
		self.wordlist += text.split()
		
	def clearAll(self):
		"""clear EVERYTHING - can be nice when switching files to get
		rid of history that wouldn't be relevant in a backtrace for example"""
		self.wordlist = []
		self.pos = 0
		# stack of previous contexts (wordlist,pos); can be popped back to
		self.contexts = []

	def pushWords(self, words: list[str]):
		"push current context and switch to new context"
		self.contexts.append((self.wordlist,self.pos))
		self.wordlist = words
		self.pos = 0

	def popWords(self):
		"return to previous context, discarding current context"
		self.wordlist,self.pos = self.contexts.pop()

	def hasPushedWords(self):
		"are there wordlists left on the stack?"
		return len(self.contexts) > 0
	
	def nextWord(self):
		"get next word or None"
		if self.pos >= len(self.wordlist):
			return None
		else:
			w = self.wordlist[self.pos]
			self.pos += 1
			return w
			
	def peekWord(self):
		"peek at next word or None"
		if self.pos >= len(self.wordlist):
			return None
		else:
			return self.wordlist[self.pos]
			
	def prevWord(self):
		"get previous word or None"
		if self.pos <= 0:
			return None
		else:
			self.pos -= 1
			return self.wordlist[self.pos]
			
	def deletePrevWord(self):
		"delete the word before the current position in the stream"
		if self.pos == 0: 
			raise LangError("No previous word to delete!")
		
		del self.wordlist[self.pos-1]
		self.pos -= 1

	def insertPrevWord(self, word):
		"""insert a word before the current position (would be read by 
		a subsequent prevWord())"""
		self.wordlist.insert(self.pos, word)
		self.pos += 1
