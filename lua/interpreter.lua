--[[
	Interpreter - runs code.

	There is no compilation step, not really even a parsing step -- the interpreter
	runs directly from the wordlists from the Reader. This makes the code smaller and
	makes e.g. forward declarations really easy since nothing is evaluated until it
	runs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the Python implementation.
]]

require("native")
require("syntax")
require("langtypes")

Interpreter = {}

function Interpreter:addText(text)
	self.syntax:addText(text)
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
	if val > MAX_INT_31 or val < MIN_INT_31 then
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

function Interpreter:do_jump(jumpword)
	--print("DO JUMP " .. jumpword)
	-- take word like '>>NAME' or '<<NAME' and jump to '@NAME'
	if string.sub(jumpword,1,2) == ">>" then
		-- forward jump, find word (>>NAME -> @NAME)
		while true do
			local word = self.syntax:nextObjOrFail()
			--print("WORD " .. fmtStackPrint(word))
			if isSymbol(word) and string.sub(word,2) == string.sub(jumpword,3) then
				return -- found word, stop
			end
		end
	elseif string.sub(jumpword,1,2) == "<<" then
		-- backward jump
		while true do
			local word = self.syntax:prevObjOrFail()
			--print("FIND BACKWARD: " .. jumpword .. ", " .. word)
			if isSymbol(word) and string.sub(word,2) == string.sub(jumpword,3) then
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

		local obj = self.syntax:nextObj()
		--print("RUN OBJ: " .. fmtStackPrint(obj))

		if stephook ~= nil then
			stephook(self,word)
		end
		
		if obj == "" then
			--print("RETURNING FROM WORD")
			-- i could be returning from a word that had no 'return',
			-- so pop words like i would if it were a return
			if self.syntax:hasPushedObjLists() then
				self.syntax:popObjList()
					goto MAINLOOP
			else
				return
			end
		end

		-- see if its a literal to push
		if type(obj) == "number" or isFloat(obj) or isString(obj) or isCallableWordlist(obj) then
			self:push(obj)
			goto MAINLOOP
		end

		if isSymbol(obj) then
			if obj == "return" then
				-- return from word by popping back to previous wordlist (if not at toplevel)
				if self.syntax:hasPushedObjLists() then
					self.syntax:popObjList()
				else
					return -- return from top level exits program
				end
				goto MAINLOOP
			end

			if obj == "if" then
				-- true jump is required
				local true_jump = self.syntax:nextObj()
				-- false word is optional
				local peeked = self.syntax:peekObj()
				--print("PEEKED:" .. fmtStackPrint(peeked))
				local false_jump = nil
				if isSymbol(peeked) then
					if string.sub(peeked,1,2) == "<<" or string.sub(peeked,1,2) == ">>" then
						false_jump = self.syntax:nextObj()
					end
				end
				--print("TRUE JUMP: " .. fmtStackPrint(true_jump))
				--print("FALSE JUMP: " .. fmtStackPrint(false_jump))
				-- get true|false condition
				local cond = self:pop()
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

			if string.sub(obj, 1, 2) == ">>" or string.sub(obj, 1, 2) == "<<" then
				self:do_jump(obj)
				goto MAINLOOP
			end

			if string.sub(obj, 1, 1) == "@" then
				-- jump target -- ignore
				goto MAINLOOP
			end

			if obj == "var" then
				name = self.syntax:nextObjOrFail()
				count = self.syntax:nextObjOrFail()
				if not type(count) == "number" then
					error(">>>Expected int after var name but got " .. fmtStackPrint(count))
				end
				-- must be unique nmw
				if self.VARS[name] ~= nil then
					error(">>>Trying to redefine variable "  .. name)
				end
			
				self.VARS[name] = new_MemArray(count)
				goto MAINLOOP
			end
				
			if obj == "del" then
				name = self.syntax:nextObjOrFail()
				if self.VARS[name] == nil then
					error(">>>Trying to delete non-existent variable " .. name)
				end
				
				self.VARS[name] = nil
				goto MAINLOOP
			end
				
			if obj == "call" then
				-- top of stack must be a CallableWordlist
				obj = self:pop()
				if not isCallableWordlist(obj) then
					error(">>>call expects a lambda, but got: " .. fmtStackPrint(obj))
				end

				-- now this is just like calling a userword, below
				-- TODO -- tail call elimination??
				self.syntax:pushObjList(obj.wordlist)
				goto MAINLOOP
			end

			-- builtins then userwords then vars

			if BUILTINS[obj] ~= nil then
				argtypes = BUILTINS[obj][1]
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
				BUILTINS[obj][2](table.unpack(args))
				goto MAINLOOP
			end

			if self.WORDS[obj] ~= nil then
				self.syntax:pushObjList(self.WORDS[obj])
				--print("CALL USERWORD:" .. fmtStackPrint(obj))
				--print("NOW WORDLIST: " .. tostring(self.syntax.reader.wordlist))
				--self.syntax.reader:debug_print_wordlist()
				goto MAINLOOP
			end

			if self.VARS[obj] ~= nil then
				self:push(self.VARS[obj])
				goto MAINLOOP
			end
		end

		error(">>>Unknown word " .. fmtStackPrint(obj))
	end
end

function Interpreter:new(obj)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "Interpreter"
	
	obj.STACK_SIZE = (1<<16)
	obj.LOCALS_SIZE = (1<<10)

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

	obj.syntax = new_Syntax()

	return obj
end

function new_Interpreter()
	return Interpreter:new({})
end
