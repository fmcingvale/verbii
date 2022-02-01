"""
	Reader unittests

	I do NOT want to have unittests for everything, since I think running code in
	the interpreter is the best test. However, it's hard to test some low level details
	of the Reader just with code, so I made an exception here.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	This implements the same tests as the C++ version.
"""
from asyncore import read
import re
import unittest
from reader import Reader

class TestReader(unittest.TestCase):
	def test_forward_1(self):
		reader = Reader()
		reader.addText("one two three four")

		self.assertEqual(reader.nextWord(), "one")
		self.assertEqual(reader.nextWord(), "two")
		self.assertEqual(reader.nextWord(), "three")
		self.assertEqual(reader.nextWord(), "four")
		self.assertEqual(reader.nextWord(), None)

	def test_forward_2(self):
		reader = Reader()
		reader.addText("""   def :\nhello ."   1234   world !   """)
		self.assertEqual(reader.nextWord(), "def")
		self.assertEqual(reader.nextWord(), ":")
		self.assertEqual(reader.nextWord(), "hello")
		self.assertEqual(reader.nextWord(), ".\"")
		self.assertEqual(reader.nextWord(), "1234")
		self.assertEqual(reader.nextWord(), "world")
		self.assertEqual(reader.nextWord(), "!")
		self.assertEqual(reader.nextWord(), None)
	
	def test_forward_backward(self):
		reader = Reader()
		reader.addText("  one 123 \r\n  three 456  \t \n\r five    ")
		self.assertEqual(reader.nextWord(), "one")
		self.assertEqual(reader.nextWord(), "123")
		self.assertEqual(reader.nextWord(), "three")
		
		self.assertEqual(reader.prevWord(), "three")
		self.assertEqual(reader.prevWord(), "123")
		
		self.assertEqual(reader.nextWord(), "123")
		self.assertEqual(reader.nextWord(), "three")
		
		self.assertEqual(reader.nextWord(), "456")
		self.assertEqual(reader.nextWord(), "five")
		self.assertEqual(reader.nextWord(), None)
		
		self.assertEqual(reader.prevWord(), "five")
		self.assertEqual(reader.prevWord(), "456")
		self.assertEqual(reader.prevWord(), "three")
		self.assertEqual(reader.prevWord(), "123")
		self.assertEqual(reader.prevWord(), "one")
		self.assertEqual(reader.prevWord(), None)

	def test_push_pop(self):		
		reader = Reader()
		self.assertFalse(reader.hasPushedWords())
		reader.addText("  1-aaa  1-bbb 1-ccc  1-ddd  ")
		self.assertFalse(reader.hasPushedWords())
		
		self.assertEqual(reader.nextWord(), "1-aaa")
		self.assertEqual(reader.peekWord(), "1-bbb")
		self.assertEqual(reader.nextWord(), "1-bbb")
	
		more = ["2-aaa","2-bbb","2-ccc"]
		self.assertFalse(reader.hasPushedWords())
		reader.pushWords(more)
		self.assertTrue(reader.hasPushedWords())
		
		self.assertEqual(reader.nextWord(), "2-aaa")
		self.assertEqual(reader.nextWord(), "2-bbb")
		self.assertEqual(reader.peekWord(), "2-ccc")
		self.assertEqual(reader.nextWord(), "2-ccc")
		
		more2 = ["3-aaa","3-bbb","3-ccc"]
		self.assertTrue(reader.hasPushedWords())
		reader.pushWords(more2)
		self.assertTrue(reader.hasPushedWords())
		
		self.assertEqual(reader.nextWord(), "3-aaa")
		self.assertEqual(reader.peekWord(), "3-bbb")
		self.assertEqual(reader.nextWord(), "3-bbb")
		self.assertEqual(reader.nextWord(), "3-ccc")
		self.assertEqual(reader.peekWord(), None)
		self.assertEqual(reader.nextWord(), None)
		
		self.assertEqual(reader.prevWord(), "3-ccc")
		self.assertTrue(reader.hasPushedWords())
		reader.popWords()
		self.assertTrue(reader.hasPushedWords())
		
		self.assertEqual(reader.prevWord(), "2-ccc")
		self.assertEqual(reader.peekWord(), "2-ccc")
		self.assertEqual(reader.prevWord(), "2-bbb")
		
		self.assertTrue(reader.hasPushedWords())
		reader.popWords()
		self.assertFalse(reader.hasPushedWords())
		
		self.assertEqual(reader.nextWord(), "1-ccc")
		self.assertEqual(reader.peekWord(), "1-ddd")
		self.assertEqual(reader.nextWord(), "1-ddd")
		
	def test_insert_delete(self):
		
		reader = Reader()
		reader.addText("   one  two   three  four   ")

		self.assertEqual(reader.nextWord(), "one")
		self.assertEqual(reader.nextWord(), "two")
		reader.deletePrevWord()
		self.assertEqual(reader.nextWord(), "three")
		self.assertEqual(reader.prevWord(), "three")
		self.assertEqual(reader.prevWord(), "one")
		self.assertEqual(reader.prevWord(), None)
		
		self.assertEqual(reader.nextWord(), "one")
		self.assertEqual(reader.nextWord(), "three")
		reader.insertPrevWord("new-1")	
		self.assertEqual(reader.nextWord(), "four")
		self.assertEqual(reader.prevWord(), "four")
		self.assertEqual(reader.prevWord(), "new-1")
		self.assertEqual(reader.prevWord(), "three")
		self.assertEqual(reader.prevWord(), "one")
		reader.insertPrevWord("new-2")
		self.assertEqual(reader.prevWord(), "new-2")
		self.assertEqual(reader.nextWord(), "new-2")
		self.assertEqual(reader.nextWord(), "one")
		
		reader.clearAll()
		reader.addText("  111   222   333   ")
		self.assertEqual(reader.nextWord(), "111")
		reader.deletePrevWord()
		reader.insertPrevWord("new-1")
		reader.insertPrevWord("new-2")
		self.assertEqual(reader.prevWord(), "new-2")
		self.assertEqual(reader.prevWord(), "new-1")
		self.assertEqual(reader.prevWord(), None)
		self.assertEqual(reader.nextWord(), "new-1")
		self.assertEqual(reader.nextWord(), "new-2")
		self.assertEqual(reader.nextWord(), "222")
		self.assertEqual(reader.nextWord(), "333")
		self.assertEqual(reader.nextWord(), None)
		
if __name__ == '__main__':
	unittest.main(verbosity=2)
