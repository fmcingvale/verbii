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
		error(">>>Stack overflow")
	end
	self.SP = self.SP - 1
	self.STACKLOCALS[self.SP] = obj
end

function Interpreter:pushInt(val)
	-- like push but checks for valid integer range
	if val > self.MAX_INT_31 or val < self.MIN_INT_31 then
		error(">>>Integer overflow")
	end

	self:push(val)
end

function Interpreter:pop()
	if self.SP >= self.SP_EMPTY then
		error(">>>Stack underflow")
	end

	obj = self.STACKLOCALS[self.SP]
	self.SP = self.SP + 1
	return obj
end

function Interpreter:reprStack()
	s = ""
	i = self.SP_EMPTY-1
	while i >= self.SP do
		s = s .. fmtStackPrint(self.STACKLOCALS[i]) .. " "
		i = i - 1
	end

	return s
end

function Interpreter:nextWordOrFail()
	-- get next word, which must be non-None, or error
	word = self.reader:nextWord()
	if word == "" then
		error(">>>Unexpected end of input")
	end
	
	return word
end

function Interpreter:prevWordOrFail()
	-- get previous word, which must be non-None, or error
	word = self.reader:prevWord()
	if word == "" then
		error(">>>Unable to find previous word")
	end
	
	return word
end

function Interpreter:do_jump(jumpword)
	--print("DO JUMP " .. jumpword)
	-- take word like '>>NAME' or '<<NAME' and jump to '@NAME'
	if string.sub(jumpword,1,2) == ">>" then
		-- forward jump, find word (>>NAME -> @NAME)
		while true do
			word = self:nextWordOrFail()
			--print("WORD " .. word)
			if string.sub(word,2) == string.sub(jumpword,3) then
				return -- found word, stop
			end
		end
	elseif string.sub(jumpword,1,2) == "<<" then
		-- backward jump
		while true do
			word = self:prevWordOrFail()
			--print("FIND BACKWARD: " .. jumpword .. ", " .. word)
			if string.sub(word,2) == string.sub(jumpword,3) then
				return -- found word, stop
			end
		end
	else
		error(">>>Bad jumpword " .. jumpword)
	end
end

-- allocate count memory slots and return start address
function Interpreter:allocate(count)
	addr = self.MEM_NEXT
	self.MEM_NEXT = self.MEM_NEXT + count
	return addr
end

function Interpreter:run(stephook)
	-- run one word at a time in a loop, with the reader position as the continuation		
	while true do
		::MAINLOOP::
		-- see C++ notes on why certain words are here vs in native.py .. short story, it's pretty arbitrary

		word = self.reader:nextWord()
		if stephook ~= nil then
			stephook(self,word)
		end
		
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
			
		if string.match(word, "^[%-]?[%d]+$") then
			-- integers just get pushed to the stack
			self:pushInt(tonumber(word))
			goto MAINLOOP
		end

		if string.sub(word,1,1) == "#" then
			self:push(new_Float(tonumber(string.sub(word, 2))))
			goto MAINLOOP
		end

		local matches = string.match(word, "^%$<lambda ([%d]+)>$")
		if matches then
			index = tonumber(matches)
			if index < 0 or index > #self.LAMBDAS then
				error(">>>Bad lambda index " .. tostring(index))
			end
			self:push(self.LAMBDAS[index])
			goto MAINLOOP
		end

		if word == "return" then
			-- return from word by popping back to previous wordlist (if not at toplevel)
			if self.reader:hasPushedWords() then
				self.reader:popWords()
			else
				return -- return from top level exits program
			end
			goto MAINLOOP
		end

		if word == "if" then
			-- true jump is required
			true_jump = self.reader:nextWord()
			-- false word is optional
			if string.sub(self.reader:peekWord(),1,2) == "<<" or string.sub(self.reader:peekWord(),1,2) == ">>" then
				false_jump = self.reader:nextWord()
			else
				false_jump = nil
			end
			-- get true|false condition
			cond = self:pop()
			if cond ~= true and cond ~= false then
				error(">>>'if' expects true or false but got: " .. fmtStackPrint(cond))
			end
			-- these don't run the jump, they just reposition the reader
			if cond == true then
				self:do_jump(true_jump)
			elseif false_jump ~= nil then
				self:do_jump(false_jump)
			end
			goto MAINLOOP
		end

		if string.sub(word, 1, 2) == ">>" or string.sub(word, 1, 2) == "<<" then
			self:do_jump(word)
			goto MAINLOOP
		end

		if string.sub(word, 1, 1) == "@" then
			-- jump target -- ignore
			goto MAINLOOP
		end

		if word == "var" then
			name = self:nextWordOrFail()
			count = tonumber(self:nextWordOrFail())
			-- must be unique nmw
			if self.VARS[name] ~= nil then
				error(">>>Trying to redefine variable "  .. name)
			end
		
			self.VARS[name] = new_MemArray(count)
			goto MAINLOOP
		end
			
		if word == "del" then
			name = self:nextWordOrFail()
			if self.VARS[name] == nil then
				error(">>>Trying to delete non-existent variable " .. name)
			end
			
			self.VARS[name] = nil
			goto MAINLOOP
		end
			
		if word == "call" then
			-- top of stack must be a CallableWordlist
			obj = self:pop()
			if not isCallableWordlist(obj) then
				error(">>>call expects a lambda, but got: " .. fmtStackPrint(obj))
			end

			-- now this is just like calling a userword, below
			-- TODO -- tail call elimination??
			self.reader:pushWords(obj.wordlist)
			goto MAINLOOP
		end

		-- builtins then userwords then vars

		if BUILTINS[word] ~= nil then
			argtypes = BUILTINS[word][1]
			args = {}
			--print("POP " .. tostring(#argtypes) .. " args")
			for i=#argtypes,1,-1 do
				val = self:pop()
				if (type(val) ~= argtypes[i]) and (argtypes[i] ~= "any") then
					error(">>>Expecting type " .. argtypes[i] .. " but got " .. type(val))
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

		if self.VARS[word] ~= nil then
			self:push(self.VARS[word])
			goto MAINLOOP
		end

		error(">>>Unknown word " .. word)
	end
end

function Interpreter:new(obj)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "Interpreter"
	
	obj.STACK_SIZE = (1<<16)
	obj.LOCALS_SIZE = (1<<10)
	obj.MAX_INT_31 = (1<<30) - 1
	obj.MIN_INT_31 = -obj.MAX_INT_31
	
	obj.SIZE_STACKLOCALS = obj.STACK_SIZE + obj.LOCALS_SIZE
	obj.STACKLOCALS = {}
	-- prefill stack+locals
	for i=1,obj.SIZE_STACKLOCALS do
		obj.STACKLOCALS[i] = 0
	end

	obj.SP_MAX = obj.SIZE_STACKLOCALS - 1
	obj.SP_EMPTY = obj.SP_MAX + 1
	obj.SP = obj.SP_EMPTY
	obj.SP_MIN = obj.SP_EMPTY - obj.STACK_SIZE
	
	obj.LP_MAX = obj.SP_MIN - 1
	obj.LP_EMPTY = obj.LP_MAX + 1
	obj.LP = obj.LP_EMPTY
	obj.LP_MIN = obj.LP_EMPTY - obj.LOCALS_SIZE
	
	-- user-defined words
	obj.WORDS = {}
	-- anonymous words (referenced by index with $<lambda index> in modified wordlists)
	obj.LAMBDAS = {}
	-- vars
	obj.VARS = {}

	obj.reader = new_Reader()

	return obj
end

function new_Interpreter()
	return Interpreter:new({})
end
