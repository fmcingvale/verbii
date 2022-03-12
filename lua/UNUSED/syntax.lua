--[[
	Syntax - takes words from Reader, recognizes any syntax forms, and returns
	objects to Interpreter.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE
]]

-- still a lot of "words" vs "objects" here, for historical reasons ... just a naming thing,
-- will clean up eventually ...

require("reader")
require("langtypes")

Syntax = {}

function Syntax:addText(text)
	self.reader:addText(text)
end

function Syntax:clearAll()
	self.reader:clearAll()
end

function Syntax:pushObjList(objs)
	--print("PUSH OBJLIST:")
	--for i=1,#objs do
	--	print(fmtStackPrint(objs[i]))
	--end
	self.reader:pushWords(objs)
end

function Syntax:popObjList()
	self.reader:popWords()
end

function Syntax:hasPushedObjLists()
	return self.reader:hasPushedWords()
end

function Syntax:nextObjOrFail()
	-- get next word, which must be non-None, or error
	local obj = self:nextObj()
	if obj == "" then
		error(">>>Unexpected end of input")
	end
	
	return obj
end

function Syntax:prevObjOrFail()
	-- get previous word, which must be non-None, or error
	local obj = self:prevObj()
	if obj == "" then
		error(">>>Unable to find previous word")
	end
	
	return obj
end

function Syntax:parseLambda()
	-- Ported from the Python version -- see C++ version for full comments

	-- delete { that was just read
	self.reader:deletePrevWord()

   local wordlist = {}
   local nesting = 1
   while true do
		::LOOP::
		local word = self:nextObj()
		if word == "" then
			error(">>>Unexpected end of input inside { .. }")
		end
		-- delete the { ... } as I read it -- will replace it with a CallableWordlist
		self.reader:deletePrevWord()
		if word == "{" then
			-- if I find inner lambdas, just copy them for now and later when they are run, 
			-- this same process will happen for them
			nesting = nesting + 1
			table.insert(wordlist, word)
		elseif word == "}" then
			nesting = nesting - 1
			if nesting > 0 then
				table.insert(wordlist, word) -- part of inner lambda, so push it to wordlist
				goto LOOP
			end
			
			-- create CallableWordlist from wordlist
			local callable = new_CallableWordlist(wordlist)
			-- replace { ... } in wordlist with the lamba
			self.reader:insertPrevWord(callable)
			return callable
		else
			table.insert(wordlist,word)
		end
   end
end

function Syntax:parseComment()
	local nesting = 1
	while true do
		local w = self.reader:nextWord() -- do NOT want to process objects inside comments!
		if w == "" then
			error(">>>Unexpected end of input inside comment")
		elseif w == ")" then
			nesting = nesting - 1
			if nesting == 0 then
				return self:nextObj()
			end
		elseif w == "(" then
			nesting = nesting + 1
		end
	end
end

function Syntax:parseQuotedPrintString()
	-- ." some string here " -- rewrite to '"some string here" puts'
	local s = ""
	-- delete ."
	self.reader:deletePrevWord()
	while(true) do
		local obj = self.reader:nextWord() -- *NOT* Syntax::nextObj() - don't want any processing, i.e.
										  -- don't want numbers in middle of string to be converted to ints
		if(obj == "") then
			error(">>>Unexpected end of input inside .\"")
		end

		self.reader:deletePrevWord() -- delete each obj as I read it, will replace at end
		if obj == "\"" then
			-- end of string - write new code
			self.reader:insertPrevWord(new_String(s));
			-- use only builtin functions so it works even with -noinit
			self.reader:insertPrevWord("puts");
			-- (will have blank at end, due to concatenation code below, so don't need to add one)
			-- since i rewrote into multiple objects (unlike the lambda case above), 
			-- backup the reader to string and return it
			self.reader:prevWord()
			self.reader:prevWord()
			return self.reader:nextWord()
		elseif not isSymbol(obj) then
			error(">>>Got non-symbol from Reader (!!):" .. fmtStackPrint(obj))
		else
			-- since i called reader.nextObj(), this will always be a symbol
			s  = s .. obj .. " "
		end
	end
end

function Syntax:nextObj() 
	local obj = self.reader:nextWord()
	
	if(isSymbol(obj)) then
		-- TODO -- do I need to get rid of .match like i got rid of regexes in other ports?
		-- i think .match is supposed to be lighter weight, so may not matter as much here
		if string.match(obj, "^[%-]?[%d]+$") then
			-- integer
			local intobj = tonumber(obj)
			if intobj > MAX_INT_31 or intobj < MIN_INT_31 then
				error(">>>Integer overflow")
			end
			self.reader:deletePrevWord()
			self.reader:insertPrevWord(intobj)
			return intobj
		end

		if string.sub(obj,1,1) == "#" then
			floatobj = new_Float(tonumber(string.sub(obj, 2)))
			self.reader:deletePrevWord()
			self.reader:insertPrevWord(floatobj)
			return floatobj
		end

		if obj == "{" then
			return self:parseLambda()
		elseif obj == "(" then
			return self:parseComment()
		elseif obj == ".\"" then
			return self:parseQuotedPrintString()
		end
	end

	return obj -- not syntax, return as-is
end

function Syntax:peekObj()
	-- I need the above processing to occur on peeked objects,
	-- so do it this way ....
	local obj = self:nextObj()
	if(obj ~= "") then
		self.reader:prevWord() -- rewind
	end
	return obj
end

function Syntax:prevObj()
	return self.reader:prevWord()
end

function Syntax:peekPrevObj()
	return self.reader:peekPrevWord()
end

function Syntax:new(obj)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "Syntax"
	obj.reader = new_Reader()
	return obj
end

function new_Syntax()
	return Syntax:new({})
end