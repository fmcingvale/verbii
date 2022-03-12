--[[
	Reader - splits text into words and provides next/previous interface.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	This was ported from the Python implementation.
]]

Reader = {}

function Reader:addText(text)
	for word in string.gmatch(text, "[^%s]+") do
		table.insert(self.wordlist, word)
	end
end

function Reader:clearAll()
	self.wordlist = {}
	self.pos = 1
	self.stack = {}
end

function Reader:nextWord()
	if self.pos > #self.wordlist then
		return ""
	else
		self.pos = self.pos + 1
		return self.wordlist[self.pos-1]
	end
end

function Reader:peekWord()
	if self.pos > #self.wordlist then
		return ""
	else
		return self.wordlist[self.pos]
	end
end

function Reader:prevWord()
	if self.pos <= 1 then
		return ""
	else
		self.pos = self.pos - 1
		return self.wordlist[self.pos]
	end
end

function Reader:pushWords(words)
	--print("READER PUSH WORDS:")
	--for i=1,#words do
	--	print(fmtStackPrint(words[i]))
	--end
	table.insert(self.stack, {self.wordlist,self.pos})
	self.wordlist = words
	--self:debug_print_wordlist()
	self.pos = 1
end

function Reader:hasPushedWords(word)
	return #self.stack >= 1
end

function Reader:popWords(words)
	if #self.stack < 1 then
		error(">>>Empty word stack, cannot pop")
	end
	entry = table.remove(self.stack)
	self.wordlist = entry[1]
	self.pos = entry[2]
end

function Reader:deletePrevWord()
	if self.pos <= 1 then
		error(">>>No previous word to delete")
	end
	table.remove(self.wordlist, self.pos-1)
	self.pos = self.pos - 1
end

function Reader:insertPrevWord(word)
	table.insert(self.wordlist, self.pos, word)
	self.pos = self.pos + 1
end

function Reader:debug_print_wordlist()
	print("CURRENT WORDLIST:")
	for i=1,#self.wordlist do
		print(fmtStackPrint(self.wordlist[i]))
	end
end

function Reader:new(obj)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "Reader"
	obj:clearAll() -- set other fields
	return obj
end

function new_Reader()
	return Reader:new({})
end



