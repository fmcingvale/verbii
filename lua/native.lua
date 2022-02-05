--[[
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from Python version
]]

function int_divmod(a, b)
	-- see notes in C++ implementation of this function.
	--  this returns (quotient,mod) instead of taking mod as a return param.
	if b == 0 then
		error(">>>Divide by zero")
	end
		
	quot = math.floor(math.abs(a) / math.abs(b))
		
	if (a < 0 and b < 0) or (a >=0 and b >= 0) then
		samesign = true
	else
		samesign = false
	end
	
	if samesign then
		mod = a - quot*b
		return quot,mod
	else
		mod = a + quot*b
		return -quot,mod
	end
end

function builtin_divmod(intr, a, b)
	quot,mod = int_divmod(a,b)
	intr:pushInt(mod)
	intr:pushInt(quot)
end

function isCallableWordlist(obj)
	return type(obj) == "table" and obj.__class__ == "CallableWordlist"
end

function reprObject(obj)
	if type(obj) == "number" then
		return tostring(obj)
	elseif type(obj) == "boolean" then
		if obj then
			return "true"
		else
			return "false"
		end
	elseif isCallableWordlist(obj) then
		return "<lambda>"
	else
		error(">>>Don't know how to print object: " .. tostring(obj))
	end
end

function builtin_repr(intr)
	obj = intr:pop()
	io.write(reprObject(obj))
end

function builtin_print_string(intr)
	while true do
		word = intr:nextWordOrFail()
		if word == "\"" then
			return
		else
			io.write(word .. " ")
		end
	end
end

function builtin_define_word(intr)
	name = intr:nextWordOrFail()
	words = {}
	while true do
		w = intr:nextWordOrFail()
		--print("DEFINE WORD:" .. w)
		if w == ';' then
			intr.WORDS[name] = words
			return
		else
			table.insert(words, w)
		end
	end
end

-- ( obj addr -- ) - save obj to addr
function builtin_set(intr, obj, addr)
	if addr < 0 or addr >= intr.MEM_NEXT then
		error(">>>Bad address in set!: " .. tostring(addr))
	end

	intr.RAM[addr] = obj -- like Python, can just store obj directly
end

-- ( addr -- obj ) load obj from addr and push to stack
function builtin_ref(intr, addr)
	if addr < 0 or addr >= intr.MEM_NEXT then
		error(">>>Bad address in ref: " .. tostring(addr))
	end
	intr:push(intr.RAM[addr])
end

-- set stack pointer from addr on stack
function builtin_setsp(intr, addr)
	if addr < 0 or addr > intr.SP_EMPTY then
		error(">>>Bad address in SP!: " .. tostring(addr))
	end	
	intr.SP = addr
end

-- set locals pointer from addr on stack
function builtin_setlp(intr, addr)
	if addr < intr.LP_MIN or addr > intr.LP_EMPTY then
		error(">>>Bad address in LP!: " .. tostring(addr))
	end
	intr.LP = addr
end

-- pop top of stack and push to locals
function builtin_tolocal(intr)
	if intr.LP <= intr.LP_MIN then
		error(">>>Locals overflow")
	end	
	intr.LP = intr.LP - 1
	intr.RAM[intr.LP] = intr:pop()
end

-- pop top locals and push to stack
function builtin_fromlocal(intr)
	if intr.LP >= intr.LP_EMPTY then
		error(">>>Locals underflow")
	end
	intr:push(intr.RAM[intr.LP])
	intr.LP = intr.LP + 1
end

function builtin_comment(intr)
	while true do
		w = intr:nextWordOrFail()
		if w == ")" then
			return
		end
	end
end

function builtin_showdef(intr)
	name = intr:nextWordOrFail()
	if intr.WORDS[name] == nil then
		print("No such word: " .. name)
		return
	end

	wordlist = intr.WORDS[name]
	io.write(name .. ": ")
	for i=1,#wordlist do
		io.write(wordlist[i] .. " ")
	end

	print(";")
end

CallableWordlist = {}
function CallableWordlist:new(obj, wordlist)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "CallableWordlist"
	obj.wordlist = wordlist
	return obj
end

function new_CallableWordlist(wordlist)
	return CallableWordlist:new({},wordlist)
end

function builtin_make_lambda(intr)
	 -- Ported from the Python version -- see C++ version for full comments

	 -- delete { that was just read
	 intr.reader:deletePrevWord()

	wordlist = {}
	nesting = 1
	while true do
		::LOOP::
		word = intr:nextWordOrFail()
		-- delete the { ... } as I read it -- will replace it with a CallableWordlist
		intr.reader:deletePrevWord()
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
			callable = new_CallableWordlist(wordlist)
			table.insert(intr.LAMBDAS, callable)
			local index = #intr.LAMBDAS
			-- replace { ... } in wordlist with $<lambda index> so a subsequent 'call'
			-- will find it
			intr.reader:insertPrevWord('$<lambda ' .. tostring(index) .. ">")
			-- the first time I see { ... }, I have to push the CallableWordlist.
			-- every subsequent time, the object will be pushed by the wordlist i just modified
			intr:push(callable)
			return
		else
			table.insert(wordlist,word)
		end
	end
end

BUILTINS = {
	["+"] = { {"number","number"}, function(intr,a,b) intr:pushInt(a+b) end },
	["-"] = { {"number","number"}, function(intr,a,b) intr:pushInt(a-b) end },
	["*"] = { {"number","number"}, function(intr,a,b) intr:pushInt(a*b) end },
	["/mod"] = { {"number","number"}, builtin_divmod },
	["=="] = { {"number","number"}, function(intr,a,b) intr:push(a==b) end },
	[">"] = { {"number","number"}, function(intr,a,b) intr:push(a>b) end },
	["repr"] = { {}, builtin_repr },
	[".\""] = {  {}, builtin_print_string },
	[".c"] = { {"number"}, function(intr,a) io.write(string.char(a)) end },
	["SP"] = { {}, function(intr) intr:push(intr.SP) end },
	["SP!"] = { {"number"}, builtin_setsp},
	["LP"] = { {}, function(intr) intr:push(intr.LP) end },
	["LP!"] = { {"number"}, builtin_setlp},
	[":"] = { {}, builtin_define_word },
	-- alias for ':'
	["def"] = { {}, builtin_define_word },
	["set!"] = { {"any","number"}, builtin_set},
	["ref"] = { {"number"}, builtin_ref },
	[">L"] = { {}, builtin_tolocal},
	["L>"] = { {}, builtin_fromlocal},
	["depth"] = { {}, function(intr) intr:push(intr.SP_EMPTY - intr.SP) end },
	["("] = { {}, builtin_comment },
	[".showdef"] = { {}, builtin_showdef},
	["{"] = { {}, builtin_make_lambda},
}
