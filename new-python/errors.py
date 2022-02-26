from __future__ import annotations
"""
	Exceptions
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
"""

class LangError(Exception):
	def __init__(self, msg):
		super().__init__()
		self.msg = msg