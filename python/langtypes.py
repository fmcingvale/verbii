from __future__ import annotations
"""
	Langtypes - Types that don't exist in the host language.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
"""

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
