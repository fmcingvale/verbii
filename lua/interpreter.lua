--[[
	Interpreter - runs byte-compiled code

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the Python implementation.
]]

require("native")
require("langtypes")

Interpreter = {}

-- '#' doesn't work on tables with non-contiguous entries, like when used as a hash table
function tableSize(t) 
	local nr = 0
	for p in pairs(t) do
		nr = nr + 1
	end
	return nr
end
	  
function Interpreter:print_stats()
	print("\n==== Runtime Stats ====")
	print("* General:")
	print("  Builtin words: " .. tostring(tableSize(BUILTINS)))
	print("  User-defined words: " .. tostring(tableSize(self.WORDS)))
	print("  Max stack depth: " .. tostring(self.SP_EMPTY - self.min_run_SP))
	print("  Max locals depth: " .. tostring(self.LP_EMPTY - self.min_run_LP))
	print("  Max callstack depth: " .. tostring(self.max_callstack))
	print("  Tail calls: " .. tostring(self.nr_tail_calls))

	print("* Lua:")
	print("  Memory in use: " .. tostring(math.ceil(1024*collectgarbage("count"))))

	print("* Notices:")
	if self.SP ~= self.SP_EMPTY then
		print("  Stack is not empty! (" .. tostring(self.SP_EMPTY-self.SP) .. " items)")
	end
	if self.LP ~= self.LP_EMPTY then
		print("  Locals are not empty! (" .. tostring(self.LP_EMPTY-self.LP) .. " items)")
	end
end

-- allocate nr object slots and returning starting index
function Interpreter:heap_alloc(nr)
	local addr = self.HEAP_NEXTFREE
	if (self.HEAP_NEXTFREE+nr) >= #self.OBJMEM then
		-- double heap when out of space
		local newsize = #self.OBJMEM * 2
		for i=self.HEAP_NEXTFREE,newsize-1 do
			self.OBJMEM[i] = 0
		end
	end

	self.HEAP_NEXTFREE = self.HEAP_NEXTFREE + nr
	return addr
end

function Interpreter:push(obj)
	-- like Python (and unlike C++), I can just push Lua objects here
	if self.SP <= self.SP_MIN then
		error(">>>Stack overflow")
	end
	self.SP = self.SP - 1
	self.OBJMEM[self.SP] = obj
	-- stats
	self.min_run_SP = math.min(self.min_run_SP, self.SP)
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

	obj = self.OBJMEM[self.SP]
	self.SP = self.SP + 1
	return obj
end

function Interpreter:reprStack()
	s = ""
	i = self.SP_EMPTY-1
	while i >= self.SP do
		s = s .. fmtStackPrint(self.OBJMEM[i]) .. " "
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
			local word = self:nextObjOrFail()
			--print("WORD " .. fmtStackPrint(word))
			if isSymbol(word) and string.sub(word,2) == string.sub(jumpword,3) then
				return -- found word, stop
			end
		end
	elseif string.sub(jumpword,1,2) == "<<" then
		-- backward jump
		while true do
			local word = self:prevObjOrFail()
			--print("FIND BACKWARD: " .. jumpword .. ", " .. word)
			if isSymbol(word) and string.sub(word,2) == string.sub(jumpword,3) then
				return -- found word, stop
			end
		end
	else
		error(">>>Bad jumpword " .. jumpword)
	end
end

function Interpreter:nextObj()
	if self.code == nil or self.codepos > #self.code then
		return nil
	else
		local obj = self.code[self.codepos]
		self.codepos = self.codepos + 1
		return obj
	end
end

function Interpreter:nextObjOrFail(why)
	local obj = self:nextObj()
	if obj == nil then
		error(">>>" .. why)
	else
		return obj
	end
end

function Interpreter:peekObj()
	if self.code == nil or self.codepos > #self.code then
		return nil
	else
		return self.code[self.codepos]
	end
end

function Interpreter:prevObj()
	if self.code == nil or self.codepos == 1 then
		return nil
	else
		self.codepos = self.codepos - 1
		return self.code[self.codepos]
	end
end

function Interpreter:prevObjOrFail(why)
	local obj = self:prevObj()
	if obj == nil then
		error(">>>" .. why)
	else
		return obj
	end
end

function Interpreter:havePrevFrames()
	return #self.callstack > 0
end

function Interpreter:code_call(objlist)
	table.insert(self.callstack, {self.code,self.codepos})
	--print("CALL - DEPTH NOW: " .. tostring(#self.callstack))
	self.code = objlist
	self.codepos = 1
	-- stats
	self.max_callstack = math.max(self.max_callstack, #self.callstack)
end

function Interpreter:code_return()
	if self:havePrevFrames() then
		local entry = table.remove(self.callstack)
		--print("RETURN - DEPTH NOW: " .. tostring(#self.callstack))
		self.code = entry[1]
		self.codepos = entry[2]
	else
		error(">>>Trying to return without call")
	end
end

function Interpreter:hasWord(name)
	return self.VARS[name] ~= nil or self.WORDS[name] ~= nil
end

function Interpreter:deleteWord(name)
	if self.VARS[name] ~= nil then
		self.VARS[name] = nil
		return
	elseif self.WORDS[name] ~= nil then
		self.WORDS[name] = nil
		return
	else
		error(">>>Trying to delete non-existent name: " .. name)
	end
end

function Interpreter:run(objlist, stephook)
	--print("** RUN: " .. fmtStackPrint(objlist))
	
	if self.code ~= nil or #self.callstack > 0 then
		error(">>>Interpreter called recursively")
	end

	self.code = objlist
	self.codepos = 1
	
	-- run one word at a time in a loop, with the reader position as the continuation		
	while true do
		::MAINLOOP::
		-- see C++ notes on why certain words are here vs in native.py .. short story, it's pretty arbitrary

		local obj = self:nextObj()
		
		--if #self.callstack < 2 then
		--print("RUN OBJ:" .. fmtStackPrint(obj)) -- .. " " .. type(obj))
		--end

		if stephook ~= nil then
			stephook(self,word)
		end
		
		if obj == nil then
			--print("RETURNING FROM WORD")
			-- i could be returning from a word that had no 'return',
			-- so pop words like i would if it were a return
			if self:havePrevFrames() then
				self:code_return()
				goto MAINLOOP
			else
				self.code = nil -- mark self as no longer running
				return
			end
		end

		-- see if its a literal to push
		if type(obj) == "number" or isFloat(obj) or isString(obj) or isLambda(obj) then
			self:push(obj)
			goto MAINLOOP
		end

		if isSymbol(obj) then
			if string.sub(obj, 1, 1) == "'" then
				-- quoted symbol, remove one level of quoting
				self:push(string.sub(obj, 2))
				goto MAINLOOP
			end

			if obj == "return" then
				-- return from word by popping back to previous wordlist (if not at toplevel)
				if self:havePrevFrames() then
					self:code_return()
				else
					self.code = nil -- mark self as stopped
					return -- return from top level exits program
				end
				goto MAINLOOP
			end

			if obj == "if" then
				-- true jump is required
				local true_jump = self:nextObj()

				-- get true|false condition
				local cond = self:pop()
				if cond ~= true and cond ~= false then
					error(">>>'if' expects true or false but got: " .. fmtStackPrint(cond))
				end
				-- this doesn't run the jump, it just repositions self.codepos
				if cond == true then
					self:do_jump(true_jump)
				end -- else, continue with next instruction after true_jump
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
				name = self:nextObjOrFail()
				count = self:nextObjOrFail()
				if not type(count) == "number" then
					error(">>>Expected int after var name but got " .. fmtStackPrint(count))
				end
				-- must be unique name
				if self:hasWord(name) then
					error(">>>Trying to redefine name: "  .. name)
				end
			
				self.VARS[name] = self:heap_alloc(count)
				goto MAINLOOP
			end
				
			if obj == "del" then
				name = self:nextObjOrFail()
				self:deleteWord(name)
				goto MAINLOOP
			end
				
			if obj == "call" then
				-- top of stack must be a Lambda
				obj = self:pop()
				if isLambda(obj) then
					-- now this is just like calling a userword, below
					-- TODO -- tail call elimination??
					self:code_call(obj.wordlist)
				elseif isList(obj) then
					self:code_call(obj)
				else
					error(">>>call expects a lambda, but got: " .. fmtStackPrint(obj))
				end

				goto MAINLOOP
			end

			-- builtins then userwords then vars

			if BUILTINS[obj] ~= nil then
				--print("BUILTIN: " .. obj)
				argtypes = BUILTINS[obj][1]
				args = {}
				--print("POP " .. tostring(#argtypes) .. " args")
				for i=#argtypes,1,-1 do
					val = self:pop()
					if (type(val) ~= argtypes[i]) and (argtypes[i] ~= "any") then
						error(">>>builtin '" .. obj .. "' expecting type " .. argtypes[i] .. " but got " .. type(val))
					end
					table.insert(args, 1, val)
				end
				table.insert(args, 1, self)
				BUILTINS[obj][2](table.unpack(args))
				goto MAINLOOP
			end

			if self.WORDS[obj] ~= nil then
				--print("READY TO CALL WORD:" .. obj)
				--print("PEEK IS:" .. fmtStackPrint(self:peekObj()))
				-- tail call elimination
				if self:peekObj() == nil or (isSymbol(self:peekObj()) and self:peekObj() == "return") then
					-- last statement in list, will never need to return here,
					-- so go ahead and pop my frame so callstack won't grow on
					-- tail-recursive calls
					if self:havePrevFrames() then
						--print("*** TAIL CALL RETURN")
						self:code_return()
						self.nr_tail_calls = self.nr_tail_calls + 1
					end
				end
				--print("WORD: " .. obj)
				self:code_call(self.WORDS[obj])
				--print("CALL USERWORD:" .. fmtStackPrint(obj))
				goto MAINLOOP
			end

			if self.VARS[obj] ~= nil then
				self:pushInt(self.VARS[obj])
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
	
	-- 3 memory areas: stack, locals, heap (program-allocated memory)
	obj.STACK_SIZE = (1<<10)
	obj.LOCALS_SIZE = (1<<10)
	obj.HEAP_STARTSIZE = (1<<16)
	
	obj.OBJMEM = {}
	-- prefill to create as correct size
	for i=1,(obj.STACK_SIZE+obj.LOCALS_SIZE+obj.HEAP_STARTSIZE) do
		obj.OBJMEM[i] = 0
	end

	-- stack first:
	obj.SP_MIN = 1	
	obj.SP_EMPTY = obj.SP_MIN + obj.STACK_SIZE
	obj.SP = obj.SP_EMPTY
	
	-- locals next:
	obj.LP_MIN = obj.SP_EMPTY
	obj.LP_EMPTY = obj.LP_MIN + obj.LOCALS_SIZE
	obj.LP = obj.LP_EMPTY
	
	-- next free heap index to allocate
	obj.HEAP_NEXTFREE = obj.LP_EMPTY

	-- code currently being run or nil for not running
	obj.code = nil
	obj.codepos = 1 -- index into code list

	-- stack of previous frames to return to
	obj.callstack = {}

	-- user-defined words
	obj.WORDS = {}
	-- vars
	obj.VARS = {}

	-- stats
	obj.max_callstack = 0
	obj.min_run_SP = obj.SP
	obj.min_run_LP = obj.LP
	obj.nr_tail_calls = 0

	return obj
end

function new_Interpreter()
	return Interpreter:new({})
end
