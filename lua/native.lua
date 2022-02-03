--[[
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from Python version
]]

function int_divmod(a, b)
	-- see notes in C++ implementation of this function.
	--  this returns (quotient,mod) instead of taking mod as a return param.
	if b == 0 then
		error("Divide by zero")
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

function reprObject(obj)
	if type(obj) == "number" then
		return tostring(obj)
	elseif type(obj) == "boolean" then
		if obj then
			return "true"
		else
			return "false"
		end
	else
		error("Don't know how to print object: " .. tostring(obj))
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

BUILTINS = {
	["+"] = { {"number","number"}, function(intr,a,b) intr:push(a+b) end },
	["-"] = { {"number","number"}, function(intr,a,b) intr:push(a-b) end },
	["*"] = { {"number","number"}, function(intr,a,b) intr:push(a*b) end },
	["/mod"] = { {"number","number"}, builtin_divmod },
	["=="] = { {"number","number"}, function(intr,a,b) intr:push(a==b) end },
	[">"] = { {"number","number"}, function(intr,a,b) intr:push(a>b) end },
	["repr"] = { {}, builtin_repr },
	[".\""] = {  {}, builtin_print_string },
	[".c"] = { {"number"}, function(intr,a) io.write(string.char(a)) end },
	["SP"] = { {}, function(intr) intr:push(intr.SP) end },
	["LP"] = { {}, function(intr) intr:push(intr.LP) end },
	[":"] = { {}, builtin_define_word },
}
