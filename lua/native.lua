--[[
	Native - functions that have to be implemented in the native (host) language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from Python version
]]
require("langtypes")

-- these are globals
ALLOW_OVERWRITING_WORDS = false
EXIT_ON_EXCEPTION = true
STACKTRACE_ON_EXCEPTION = true

-- file to write like stdout
local FILE_STDOUT = io.stdout

function current_system_tick_time()
	return os.clock()
end

STARTUP_TIME = current_system_tick_time()

function int_divmod(a, b)
	-- see notes in C++ implementation of this function.
	--  this returns (quotient,mod) instead of taking mod as a return param.
	if b == 0 then
		error(">>>Divide by zero")
	end
		
	local quot = math.floor(math.abs(a) / math.abs(b))
	local samesign = nil
	local mod = nil

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
	local quot,mod = int_divmod(a,b)
	intr:pushInt(mod)
	intr:pushInt(quot)
end

function builtin_repr(intr)
	local obj = intr:pop()
	intr:push(new_String(fmtStackPrint(obj)))
end

function builtin_str(intr)
	local obj = intr:pop()
	intr:push(new_String(fmtDisplay(obj)))
end

function builtin_puts(intr)
	local obj = intr:pop()
	if not isString(obj) then
		error(">>>puts requires string but got " .. fmtStackPrint(obj))
	end
	FILE_STDOUT:write(obj.value)
end

-- ( obj addr -- ) - save obj to addr
function builtin_set(intr, obj, addr)
	if addr < 0 or addr >= #intr.OBJMEM then
		error(">>>Bad address in set!: " .. tostring(addr))
	end

	intr.OBJMEM[addr] = obj -- like Python, can just store obj directly
end

-- ( addr -- obj ) load obj from addr and push to stack
function builtin_ref(intr, addr)
	if addr < 0 or addr >= #intr.OBJMEM then
		error(">>>Bad address in ref: " .. tostring(addr))
	end

	intr:push(intr.OBJMEM[addr])
end

-- set stack pointer from addr on stack
function builtin_setsp(intr, addr)
	if addr < 0 or addr > intr.SP_EMPTY then
		error(">>>Bad address in SP!: " .. tostring(addr))
	end	
	intr.SP = addr
	-- stats
	intr.min_run_SP = math.min(intr.min_run_SP,intr.SP)
end

function builtin_printchar(intr,a) 
	FILE_STDOUT:write(string.char(a))
	if a == 10 or a == 13 then
		FILE_STDOUT:flush()
	end
end

function popInt(intr)
	local obj = intr:pop()
	if type(obj) == "number" then
		return obj
	else
		error(">>>Expecting integer but got: " .. fmtStackPrint(obj))
	end
end

-- always returns a Float
function popFloatOrInt(intr)
	local obj = intr:pop()
	if type(obj) == "number" then
		return obj
	elseif isFloat(obj) then
		return obj.value 
	else
		error(">>>Expecting int or float but got: " .. fmtStackPrint(obj))
	end
end

function popString(intr)
	local obj = intr:pop()
	if isString(obj) then
		return obj.value
	else
		error(">>>Expecting string but got: " .. fmtStackPrint(obj))
	end
end

function popSymbol(intr)
	local obj = intr:pop()
	if type(obj) == "string" then
		return obj
	else
		error(">>>Expecting symbol but got: " .. fmtStackPrint(obj))
	end
end

function popStringOrSymbol(intr)
	local obj = intr:pop()
	if isString(obj) then
		return obj.value
	elseif type(obj) == "string" then
		return obj
	else
		error(">>>Expecting string or symbol but got: " .. fmtStackPrint(obj))
	end
end

function popType(intr, test, name)
	local obj = intr:pop()
	if test(obj) then
		return obj
	else
		error(">>>Expecting " .. name .. " but got: " .. fmtStackPrint(obj))
	end
end

function popOpcode(intr)
	return popType(intr, isOpcode, "opcode")
end

function popList(intr)
	local obj = intr:pop()
	if isList(obj) then
		return obj
	else
		error(">>>Expecting list but got: " .. fmtStackPrint(obj))
	end
end

function popLambda(intr)
	local obj = intr:pop()
	if isLambda(obj) then
		return obj
	else
		error(">>>Expecting lambda but got: " .. fmtStackPrint(obj))
	end
end

function popDict(intr)
	local obj = intr:pop()
	if isDict(obj) then
		return obj
	else
		error(">>>Expecting dict but got: " .. fmtStackPrint(obj))
	end
end

function popBool(intr)
	local obj = intr:pop()
	if isBool(obj) then
		return obj
	else
		error(">>>Expecting bool but got: " .. fmtStackPrint(obj))
	end
end

function builtin_add(intr, a, b)
	if isInt(a) and isInt(b) then
		intr:pushInt(a+b)
	elseif isNumeric(a) and isNumeric(b) then
		intr:push(new_Float(asNumeric(a) + asNumeric(b)))
	elseif isSymbol(a) and isSymbol(b) then
		intr:push(a .. b)
	elseif isString(a) and isString(b) then
		intr:push(new_String(a.value .. b.value))
	elseif isList(a) and isList(b) then
		-- not allowed to modify original lists
		local rlist = {}
		table.move(a, 1, #a, 1, rlist)
		table.move(b, 1, #b, #a+1, rlist)
		intr:push(rlist)
	else
		error(">>>Don't know how to add " .. fmtStackPrint(a) .. " (" .. type(a) .. ") and " .. fmtStackPrint(b) .. ") " .. type(b) .. ")")
	end
end

function builtin_sub(intr, a, b)
	if isInt(a) and isInt(b) then
		intr:pushInt(a-b)
	elseif isNumeric(a) and isNumeric(b) then
		intr:push(new_Float(asNumeric(a) - asNumeric(b)))
	else
		error(">>>Don't know how to subtract " .. fmtStackPrint(a) .. " (" .. type(a) .. ") and " .. fmtStackPrint(b) .. " " .. type(b) .. ")")
	end
end

function builtin_mul(intr, a, b)
	if isInt(a) and isInt(b) then
		intr:pushInt(a*b)
	elseif isNumeric(a) and isNumeric(b) then
		intr:push(new_Float(asNumeric(a) * asNumeric(b)))
	else
		error(">>>Don't know how to multiply " .. fmtStackPrint(a) .. " (" .. type(a) .. ") and " .. fmtStackPrint(b) .. " " .. type(b) .. ")")
	end
end

function builtin_div(intr, a, b)
	-- unlike above ops which preserve ints when possible, the result here is ALWAYS a float
	if isNumeric(a) and isNumeric(b) then
		if asNumeric(b) == 0 then
			error(">>>Divide by zero")
		else
			intr:push(new_Float(asNumeric(a)/asNumeric(b)))
		end
	else
		error(">>>Don't know how to divide " .. fmtStackPrint(a) .. " (" .. type(a) .. ") and " .. fmtStackPrint(b) .. " (" .. type(b) .. ")")
	end
end

function popFloatOrInt(intr)
	local o = intr:pop()
	if type(o) == "number" then
		return o
	elseif isFloat(o) then
		return o.value
	else
		error(">>>Expecting int or float but got: " .. fmtStackPrint(o))
	end
end

function builtin_make_list(intr, nr)
	local list = {}
	for i=1,nr do
		table.insert(list, 1, intr:pop())
	end
	intr:push(list)
end

function test_equal(a, b)
	if isNumeric(a) then
		return isNumeric(b) and asNumeric(a) == asNumeric(b)
	elseif isString(a) then
		return isString(b) and b.value==a.value
	elseif isSymbol(a) then
		return isSymbol(b) and a==b
	elseif isNull(a) then
		return isNull(b)
	elseif isBool(a) then
		return isBool(b) and a==b
	elseif isLambda(a) then
		return false -- lambdas never compare equal, even if same object
	elseif isBoundLambda(a) then
		return false -- same as lambdas
	elseif isOpcode(a) then
		return isOpcode(b) and a.code == b.code and a.A == b.A and 
				a.B == b.B and a.C == b.C
	elseif isVoid(a) then
		return isVoid(b)
	elseif isList(a) then
		if not isList(b) then
			return false
		elseif #a ~= #b then
			return false
		else
			for i=1,#a do
				if not test_equal(a[i],b[i]) then
					return false
				end
			end
			return true
		end
	elseif isDict(a) then
		if not isDict(b) then
			return false
		elseif dictSize(a) ~= dictSize(b) then
			return false
		else
			for k,v in pairs(a.dict) do
				if not test_equal(a.dict[k],b.dict[k]) then
					return false
				end
			end
			return true
		end
	else
		error(">>>Don't know how to compare (==) objects: " .. fmtStackPrint(a) .. " and " .. fmtStackPrint(b))
	end
end

function builtin_equal(intr, a, b)
	intr:push(test_equal(a,b))
end

function test_greater(a, b)
	-- unlike _equal(), here i can test both types intially since using > on non-comparable types is an error
	if isNumeric(a) and isNumeric(b) then return asNumeric(a) > asNumeric(b)
	elseif isString(a) and isString(b) then return a.value > b.value
	elseif isSymbol(a) and isSymbol(b) then return a > b
	elseif isList(a) and isList(b) then
		-- see c++ notes
		local nr = math.min(#a, #b)
		for i=1,nr do
			if test_greater(a[i],b[i]) then
				return true
			elseif not test_equal(a[i],b[i]) then
				return false -- a<b
			end
		end
		-- nr elements are equal, so longer list is greater
		return #a > #b
	else
		error(">>>Don't know how to compare (>) objects: " .. fmtStackPrint(a) .. " and " .. fmtStackPrint(b))
	end
end

function builtin_greater(intr, a, b)
	intr:push(test_greater(a,b))
end

function builtin_slice(intr, obj, index, nr)
	--print("SLICE: " .. fmtStackPrint(obj) .. " " .. tostring(index) .. " " .. tostring(nr) .. " " .. type(obj))
	local objsize = -1
	if isString(obj) then objsize = #obj.value
	elseif isSymbol(obj) or isList(obj) then
		objsize = #obj
	else
		error(">>>Object doesn't support slicing: " .. fmtStackPrint(obj))
	end

	if index < 0 then index = objsize + index end
	if index < 0 or index >= objsize then
		if isString(obj) then
			intr:push(new_String(""))
			return
		elseif isSymbol(obj) then
			intr:push("")
			return
		elseif isList(obj) then
			intr:push({})
			return
		end
	end

	if nr<0 then nr = objsize - index end

	--print("READY TO SLICE: INDEX=" .. tostring(index) .. " NR:" .. tostring(nr))

	if isString(obj) then
		local res = new_String(string.sub(obj.value,index+1,index+nr))
		--print("SLICED <string>: " .. res.value)
		intr:push(res)
	elseif isSymbol(obj) then
		local res = string.sub(obj,index+1,index+nr)
		--print("SLICED <symbol>: " .. res)
		intr:push(res)
	elseif isList(obj) then
		local newlist = {}
		for i=1,nr do
			table.insert(newlist, obj[index+i])
		end
		intr:push(newlist)
	end
end
	
function builtin_unmake(intr, obj)
	if isSymbol(obj) then
		for i=1,#obj do
			intr:pushInt(string.byte(obj, i))
		end
		intr:pushInt(#obj)
	elseif isString(obj) then
		for i=1,#obj.value do
			intr:pushInt(string.byte(obj.value, i))
		end
		intr:pushInt(#obj.value)
	elseif isList(obj) then
		for i=1,#obj do
			intr:push(obj[i])
		end
		intr:pushInt(#obj)
	elseif isLambda(obj) then
		-- make deepcopy - see DESIGN-NOTES.md
		intr:push(deepcopy(obj.objlist))
	end
end

function builtin_make_string(intr, nr)
	local s = ""
	for i=1,nr do
		s = string.char(popInt(intr)) .. s
	end

	intr:push(new_String(s))
end

function builtin_make_symbol(intr, nr)
	local s = ""
	for i=1,nr do
		s = string.char(popInt(intr)) .. s
	end

	intr:push(s)
end

function builtin_make_word(intr)
	local name = popSymbol(intr)
	local list = intr:pop()
	if isList(list) then
		intr:defineWord(name, list, ALLOW_OVERWRITING_WORDS)
	else
		error(">>>make-word expecting list but got: " .. fmtStackPrint(list))
	end
end

function builtin_append(intr, list, obj)
	if isList(list) then
		table.insert(list, obj)
		intr:push(list)
	else
		error(">>>append expects list but got: " .. fmtStackPrint(list))
	end
end

function builtin_extend(intr)
	local src = popList(intr)
	local dest = popList(intr)
	for i=1,#src do
		table.insert(dest, src[i])
	end
	intr:push(dest)
end

function builtin_length(intr, obj)
	if isString(obj) then intr:pushInt(#obj.value)
	elseif isSymbol(obj) then intr:pushInt(#obj)
	elseif isList(obj) then intr:pushInt(#obj)
	elseif isDict(obj) then intr:pushInt(dictSize(obj))
	else error(">>>Object does not support 'length': " .. fmtStackPrint(obj))
	end
end

function builtin_make_lambda(intr)
	local list = popList(intr)
	-- must deepcopy list - see DESIGN-NOTES.md
	intr:push(new_Lambda(deepcopy(list)))
end

function builtin_open_as_stdout(intr)
	local obj = intr:pop()
	if isVoid(obj) then
		if FILE_STDOUT ~= io.stdout then
			FILE_STDOUT:close()
			FILE_STDOUT = io.stdout
		end
	elseif isString(obj) then
		FILE_STDOUT = io.open(obj.value, "w")
	else
		error(">>>Unknown arg to open-as-stdout:" .. fmtStackPrint(obj))
	end
end

function builtin_file_read(intr)
	local filename = popString(intr)
	local f = io.open(filename, "r")
	if f == nil then
		error(">>>No such file: " .. filename)
	end
	local buf = f:read("a")
	io.close(f)
	intr:push(new_String(buf))
end

function builtin_get(intr)
	local index = intr:pop()
	local obj = intr:pop()
	if isString(obj) then
		if not isInt(index) then
			error(">>>get (string) expects integer index")
		end
		if index < 0 then index = index + #obj.value end -- negative index counts from end
		if index < 0 or index >= #obj.value then
			intr:push(new_Void()) -- out of bounds == void
		else
			intr:push(new_String(string.sub(obj.value, index+1, index+1)))
		end
	elseif isSymbol(obj) then
		if not isInt(index) then
			error(">>>get (symbol) expects integer index")
		end
		if index < 0 then index = index + #obj end
		if index < 0 or index >= #obj then
			intr:push(new_Void()) -- out of bounds == void
		else
			intr:push(string.sub(obj, index+1, index+1))
		end
	elseif isList(obj) then
		if not isInt(index) then
			error(">>>get (list) expects integer index")
		end
		if index < 0 then index = index + #obj end
		if index < 0 or index >= #obj then
			intr:push(new_Void()) -- out of bounds == void
		else
			intr:push(obj[index+1])
		end
	elseif isDict(obj) then
		if not isString(index) then
			error(">>>get (dict) expects string key, got: " .. fmtStackPrint(index))
		elseif obj.dict[index.value] == nil then
			intr:push(new_Void()) -- return void when key doesn't exist
		else
			intr:push(obj.dict[index.value])
		end
	else
		error(">>>Object does not support get: " .. fmtStackPrint(obj))
	end
end

function builtin_put(intr)
	local obj = intr:pop()
	local index = intr:pop()
	local dest = intr:pop()
	if isList(dest) then
		if not isInt(index) then
			error(">>>put expects integer index")
		end
		if index < 0 then index = index + #dest end -- negative indexes
		if index < 0 or index >= #dest then
			error(">>>Index out of range in put")
		else
			dest[index+1] = obj
			intr:push(dest)
		end
	elseif isDict(dest) then
		if not isString(index) then
			error(">>>put expects string key")
		else
			-- NOTE! key stored as the lua string, NOT verbii string, so have
			-- to convert back as needed (e.g. in keys() function)
			dest.dict[index.value] = obj
			intr:push(dest)
		end
	else
		error(">>>Object does not support put: " .. fmtStackPrint(dest))
	end
end

-- does filename exist AND is a regular file
function file_exists(filename)
	-- lua lacks a stat()-type function, so do it like this ...
	local h = io.open(filename)
	local r = false
	if io.type(h) == "file" then
		r = true
	end
	if h ~= nil then
		io.close(h)
	end
	return r
end

function builtin_file_exists(intr)
	intr:push(file_exists(popString(intr)))
end

function builtin_deserialize(intr)
	local f = io.open(popString(intr), "r")
	deserialize_stream(intr, f)
	f:close()
	-- no return, just loads words into interpreter
end

function builtin_prompt(intr)
	local prompt = popString(intr)
	io.write(prompt)
	io.flush()
	local line = io.read("l")
	if line == nil then
		intr:push(new_Void())
	else
		intr:push(new_String(line))
	end
end

function builtin_file_write(intr)
	local text = popString(intr)
	local filename = popString(intr)
	local f = io.open(filename, "w")
	f:write(text)
	f:close()
end

function builtin_file_append(intr)
	local text = popString(intr)
	local filename = popString(intr)
	local f = io.open(filename, "a")
	f:write(text)
	f:close()
end

function builtin_file_delete(intr)
	local filename = popString(intr)
	if file_exists(filename) then
		os.remove(filename)
	end
end

function builtin_keys(intr)
	local dict = popDict(intr)
	intr:push(dictKeysStringObjects(dict.dict))
end

function builtin_wordlist(intr)
	intr:push(dictKeys(intr._WORDS))
end

function builtin_make_opcode(intr)
	local C = popInt(intr)
	local B = popInt(intr)
	local A = popInt(intr)
	local name = popStringOrSymbol(intr)

	-- range checks
	if A < 0 or A > 255 then
		error("A must be [0-255] in make-opcode, got: " .. tostring(A))
	end

	if B < 0 or B > 65535 then
		error("B must be [0-65535] in make-opcode, got: " .. tostring(B))
	end

	if C < 0 or C > 0x000fffff then
		error("C must be [0-1048575] in make-opcode, got: " .. tostring(C))
	end

	intr:push(new_Opcode(opcode_name_to_code(name), A, B, C))
end

function builtin_opcode_packed(intr)
	local op = popOpcode(intr)
	intr:push(op:packed())
end

function builtin_bind_lambda(intr)
	local lambda = popLambda(intr)
	-- bind lambda to current frame -- mark frame as bound so interpreter
	-- knows it cannot be freed on return
	intr.framedata.bound = true
	intr:push(new_BoundLambda(lambda.objlist, intr.framedata))
end

function builtin_file_pathsep(intr)
	intr:push(new_String("/")) -- assume posix for now
end

function builtin_getcwd(intr)
	-- from: https://www.programming-idioms.org/idiom/106/get-program-working-directory/3804/lua
	-- hackish way to avoid having to use external library
	-- the getenv() is for linux; the popen() part is for windows
	local dir = os.getenv("PWD") or io.popen("cd"):read()
	intr:push(new_String(dir))
end

function builtin_atan2(intr)
	local x = popFloatOrInt(intr)
	local y = popFloatOrInt(intr)
	-- called atan in stock lua (5.4.x at least)
	intr:push(new_Float(math.atan(y, x)))
end

function builtin_pow(intr)
	local y = popFloatOrInt(intr)
	local x = popFloatOrInt(intr)
	-- apparently math.pow is deprecated? or was?
	intr:push(new_Float(x ^ y))
end

-- see c++ version for details
function builtin_fnv_1a_32(intr)
	local s = popString(intr)
	local hash = 2166136261
	for i=1,#s do
		hash = hash ~ string.byte(s, i)
		hash = hash * 16777619
		hash = hash & 0xffffffff
	end
	intr:push(hash)
end
	
function builtin_time_string(intr)
	local d = os.date('*t')
	intr:push(new_String(string.format('%d-%02d-%02d %02d:%02d:%02d', d.year, d.month, d.day, d.hour, d.min, d.sec)))
end

-- this is global so interpreter can access
BUILTINS = {
	["+"] = { {"any","any"}, builtin_add },
	["-"] = { {"any","any" }, builtin_sub },
	["*"] = { {"any","any" }, builtin_mul },
	["/"] = { {"any","any" }, builtin_div },
	["/mod"] = { {"number","number"}, builtin_divmod },
	["f.setprec"] = { {"number"}, function(intr,a) FLOAT_PRECISION = a end },
	["=="] = { {"any","any"}, builtin_equal },
	[">"] = { {"any","any"}, builtin_greater },
	["int?"] = { {"any"}, function(intr,o) intr:push(isInt(o)) end},
	["float?"] = { {"any"}, function(intr,o) intr:push(isFloat(o)) end},
	["bool?"] = { {"any"}, function(intr,o) intr:push(isBool(o)) end},
	["null?"] = { {"any"}, function(intr,o) intr:push(isNull(o)) end},
	["void?"] = { {"any"}, function(intr,o) intr:push(isVoid(o)) end},
	["list?"] = { {"any"}, function(intr,o) intr:push(isList(o)) end},
	["string?"] = { {"any"}, function(intr,o) intr:push(isString(o)) end},
	["symbol?"] = { {"any"}, function(intr,o) intr:push(isSymbol(o)) end},
	["lambda?"] = { {"any"}, function(intr,o) intr:push(isLambda(o)) end},
	["bound-lambda?"] = { {"any"}, function(intr,o) intr:push(isBoundLambda(o)) end},
	["opcode?"] = { {"any"}, function(intr,o) intr:push(isOpcode(o)) end},
	["repr"] = { {}, builtin_repr },
	["str"] = { {}, builtin_str },
	["puts"] = { {}, builtin_puts },
	[".c"] = { {"number"}, builtin_printchar },
	["SP"] = { {}, function(intr) intr:push(intr.SP) end },
	["SP!"] = { {"number"}, builtin_setsp},
	["set!"] = { {"any","number"}, builtin_set},
	["ref"] = { {"number"}, builtin_ref },
	["depth"] = { {}, function(intr) intr:push(intr.SP_EMPTY - intr.SP) end },
	
	["make-list"] = { {"number"}, builtin_make_list},
	["slice"] = { {"any","number","number"}, builtin_slice},
	["unmake"] = { {"any"}, builtin_unmake},
	["make-string"] = { {"number"}, builtin_make_string},
	["length"] = { {"any"}, builtin_length},
	["make-word"] = { {}, builtin_make_word},
	["append"] = { {"any","any"}, builtin_append},
	["extend"] = { {}, builtin_extend},
	["parse-int"] = { {}, function(intr) intr:pushInt(tonumber(popStringOrSymbol(intr))) end},
	["parse-float"] = { {}, function(intr) intr:push(new_Float(tonumber(popStringOrSymbol(intr)))) end},
	["make-symbol"] = { {"number"}, builtin_make_symbol},
	["make-lambda"] = { {}, builtin_make_lambda},
	[".dumpword"] = { {}, function(intr) intr:push(deepcopy(intr:lookupWordOrFail(popSymbol(intr)))) end},
	[".wordlist"] = { {}, builtin_wordlist},
	["void"] = { {}, function(intr) intr:push(new_Void()) end},
	["error"] = { {}, function(intr) error(">>>" .. popString(intr)) end},
	
	["put"] = { {}, builtin_put},
	["get"] = { {}, builtin_get},
	["deepcopy"] = { {"any"}, function(intr,obj) intr:push(deepcopy(obj)) end},
	["alloc"] = { {"number"}, function(intr,nr) intr:push(intr:heap_alloc(nr)) end},
	[",,del"] = { {}, function(intr) intr:deleteWord(popSymbol(intr)) end},
	["bit-and"] = { {"number","number"}, function(intr,a,b) intr:pushInt((a&b) & 0xffffffff) end},
	["bit-or"] = { {"number","number"}, function(intr,a,b) intr:pushInt((a|b) & 0xffffffff) end},
	["bit-xor"] = { {"number","number"}, function(intr,a,b) intr:pushInt((a~b) & 0xffffffff) end},
	["bit-not"] ={ {"number"}, function(intr,a) intr:pushInt((~a) & 0xffffffff) end},
	["bit-shl"] = { {"number","number"}, function(intr,a,n) intr:pushInt((a<<n) & 0xffffffff) end},
	["bit-shr"] = { {"number","number"}, function(intr,a,n) intr:pushInt((a>>n) & 0xffffffff) end},
	
	["cpu-time"] = { {}, function(intr) intr:push(new_Float(current_system_tick_time() - STARTUP_TIME)) end},
	[",,new-dict"] = { {}, function(intr) intr:push(new_Dict()) end},

	-- new words needed for running boot.verb
	["file-exists?"] = { {}, builtin_file_exists},
	["open-as-stdout"] = { {}, builtin_open_as_stdout},
	["deserialize"] = { {}, builtin_deserialize},
	["prompt"] = { {}, builtin_prompt},
	["set-exit-on-exception"] = { {}, function(intr) EXIT_ON_EXCEPTION = popBool(intr) end},
	["set-allow-overwrite-words"] = { {}, function(intr) ALLOW_OVERWRITING_WORDS = popBool(intr) end},
	["set-stacktrace-on-exception"] = { {}, function(intr) STACKTRACE_ON_EXCEPTION = popBool(intr) end},
	
	["time-string"] = { {}, builtin_time_string},
	["floor"] = { {}, function(intr) intr:pushInt(math.floor(popFloatOrInt(intr))) end},

	["file-write"] = { {}, builtin_file_write},
	["file-append"] = { {}, builtin_file_append},
	["file-read"] = { {}, builtin_file_read},
	["file-delete"] = { {}, builtin_file_delete},	

	["sys-platform"] = { {}, function(intr) intr:push(new_String(_VERSION)) end},

	["keys"] = { {}, builtin_keys },

	["sin"] = { {}, function(intr) intr:push(new_Float(math.sin(popFloatOrInt(intr)))) end},
	["cos"] = { {}, function(intr) intr:push(new_Float(math.cos(popFloatOrInt(intr)))) end},
	["tan"] = { {}, function(intr) intr:push(new_Float(math.tan(popFloatOrInt(intr)))) end},
	["asin"] = { {}, function(intr) intr:push(new_Float(math.asin(popFloatOrInt(intr)))) end},
	["acos"] = { {}, function(intr) intr:push(new_Float(math.acos(popFloatOrInt(intr)))) end},
	["atan2"] = { {}, builtin_atan2},
	["sqrt"] = { {}, function(intr) intr:push(new_Float(math.sqrt(popFloatOrInt(intr)))) end},
	["log"] = { {}, function(intr) intr:push(new_Float(math.log(popFloatOrInt(intr)))) end},
	["exp"] = { {}, function(intr) intr:push(new_Float(math.exp(popFloatOrInt(intr)))) end},
	["pow"] = { {}, builtin_pow},

	["make-opcode"] = { {}, builtin_make_opcode },
	["opcode-packed"] = { {}, builtin_opcode_packed },
	["bind-lambda"] = { {}, builtin_bind_lambda },

	["file-pathsep"] = { {}, builtin_file_pathsep },
	["os-getcwd"] = { {}, builtin_getcwd },

	["fnv-1a-32"] = { {}, builtin_fnv_1a_32 },
}
