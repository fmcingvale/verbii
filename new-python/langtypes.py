from __future__ import annotations
"""
	Langtypes - Types that don't exist in the host language.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
"""

FLOAT_PRECISION = 17
MAX_INT_31 = (1<<30) - 1
MIN_INT_31 = -MAX_INT_31

class MemArray(object):
	def __init__(self, count, offset):
		self.mem = [0]*count
		self.offset = 0

	def clone(self):
		"return copy that shares .mem but has own offset"
		c = MemArray(0,0)
		c.mem = self.mem
		c.offset = self.offset
		return c

# since symbols are far more common than strings,
# Python strings are used for symbols and this class is used
# for strings
class LangString(object):
	def __init__(self, s):
		self.s = s

class LangLambda(object):
	"from { ... } - a lambda/anonymous word"
	def __init__(self, wordlist):
		self.wordlist = wordlist

	def __str__(self):
		return "<lambda>"

def fmtDisplay(obj):
	"get obj as string for normal program output (like '.')"
	if type(obj) is int:
		return str(obj)
	elif type(obj) is float:
		fmt = "{0:." + str(FLOAT_PRECISION) + "g}"
		return fmt.format(obj)
	elif obj is True:
		return "true"
	elif obj is False:
		return "false"
	elif isinstance(obj, LangLambda):
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
	if type(obj) is int:
		return str(obj)
	elif type(obj) is float:
		fmt = "{0:." + str(FLOAT_PRECISION) + "g}"
		return '#' + fmt.format(obj)
	elif obj is True:
		return "true"
	elif obj is False:
		return "false"
	elif isinstance(obj, LangLambda):
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
