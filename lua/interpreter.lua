--[[
	Interpreter - runs code.

	There is no compilation step, not really even a parsing step -- the interpreter
	runs directly from the wordlists from the Reader. This makes the code smaller and
	makes e.g. forward declarations really easy since nothing is evaluated until it
	runs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the Python implementation.
]]

require("reader")
require("native")

Interpreter = {}

function Interpreter:addText(text)
	self.reader:addText(text)
end

function Interpreter:push(obj)
	-- like Python (and unlike C++), I can just push Lua objects here
	if self.SP <= self.SP_MIN then
		error("Stack overflow")
	end
	self.SP = self.SP - 1
	self.RAM[self.SP] = obj
end

function Interpreter:pushInt(val)
	-- like push but checks for valid integer range
	if val > self.MAX_INT_31 or val < self.MIN_INT_31 then
		error("Integer overflow")
	end

	self:push(val)
end

function Interpreter:pop()
	if self.SP >= self.SP_EMPTY then
		error("Stack underflow")
	end

	obj = self.RAM[self.SP]
	self.SP = self.SP + 1
	return obj
end

function Interpreter:reprStack()
	-- TODO -- fixme to print objects like in C++ version
	s = ""
	i = self.SP_EMPTY-1
	while i >= self.SP do
		s = s .. string.format("%q", self.RAM[i]) .. " "
		i = i - 1
	end

	return s
end

function Interpreter:nextWordOrFail()
	-- get next word, which must be non-None, or error
	word = self.reader:nextWord()
	if word == "" then
		error ("Unexpected end of input")
	end
	
	return word
end

function Interpreter:prevWordOrFail()
	-- get previous word, which must be non-None, or error
	word = self.reader:prevWord()
	if word == "" then
		error ("Unable to find previous word")
	end
	
	return word
end

function Interpreter:run()
	-- run one word at a time in a loop, with the reader position as the continuation		
	while true do
		::MAINLOOP::
		-- see C++ notes on why certain words are here vs in native.py .. short story, it's pretty arbitrary

		word = self.reader:nextWord()
		
		if word == "" then
			-- i could be returning from a word that had no 'return',
			-- so pop words like i would if it were a return
			if self.reader:hasPushedWords() then
				self.reader:popWords()
					goto MAINLOOP
			else
				return
			end
		end
			
		if string.match(word, "^[%d]+$") then
			-- integers just get pushed to the stack
			self:pushInt(tonumber(word))
			goto MAINLOOP
		end

		if BUILTINS[word] ~= nil then
			argtypes = BUILTINS[word][1]
			args = {}
			--print("POP " .. tostring(#argtypes) .. " args")
			for i=#argtypes,1,-1 do
				val = self:pop()
				if type(val) ~= argtypes[i] then
					error("Expecting type " .. argtypes[i] .. " but got " .. type(val))
				end
				table.insert(args, 1, val)
			end
			table.insert(args, 1, self)
			BUILTINS[word][2](table.unpack(args))
			goto MAINLOOP
		end

		if self.WORDS[word] ~= nil then
			self.reader:pushWords(self.WORDS[word])
			goto MAINLOOP
		end

		error("Unknown word " .. word)
	end
end

function Interpreter:new(obj)
	setmetatable(obj, self)
	self.__index = self
	
	obj.STACK_SIZE = (1<<16)
	obj.LOCALS_SIZE = (1<<10)
	obj.MAX_INT_31 = (1<<30) - 1
	obj.MIN_INT_31 = -obj.MAX_INT_31
	
	--[[ 
		pre-creating a large array in Lua doesn't seem as straightforward as in Python
		or C++ ... I think I'd have to fill in in a loop ... however, the table will
		grow automatically, so I'm putting the stacks/locals at the start and then free
		memory will grow on top on that ]]
	obj.RAM = {}
	obj.SP_MIN = 1
	obj.SP_MAX = obj.SP_MIN + obj.STACK_SIZE - 1
	obj.SP_EMPTY = obj.SP_MAX + 1
	obj.SP = obj.SP_EMPTY

	-- prefill stack
	for i=obj.SP_MIN,obj.SP_MAX do
		obj.RAM[i] = 0
	end

	obj.LP_MIN = obj.SP_MAX + 1
	obj.LP_MAX = obj.LP_MIN + obj.LOCALS_SIZE - 1
	obj.LP_EMPTY = obj.LP_MAX + 1
	obj.LP = obj.LP_EMPTY

	-- prefill locals
	for i=obj.LP_MIN,obj.LP_MAX do
		obj.RAM[i] = 0
	end
	
	obj.MEM_NEXT = obj.LP_MAX + 1
	-- no max ram size, just allow to keep growing

	-- user-defined words
	obj.WORDS = {}
	
	obj.reader = new_Reader()

	return obj
end

function new_Interpreter()
	return Interpreter:new({})
end
