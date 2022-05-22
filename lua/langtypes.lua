--[[
	LangTypes - types that can't be represented with native Lua types.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Complete typemappings:

	verbii		Lua
	------		---
	null		Null (class)
	void		Void (class)
	int			number
	float		Float (class)
	boolean		boolean
	symbol		string	** since symbols are more commonly used in the Lua source, then
						** like Python, use Lua strings for symbols
	string		String (class)
	lambda		Lambda (class)
	list		table (i.e. plain table with no __class attribute)
--]]

-- it is tempting to use nil for verbii nil, but like other ports (Python, Chicken),
-- this is avoiding to not clash in semantics with Lua nil usage. A primary example
-- is for testing for a key in a table like "if table[key] ~= nil" -- this would
-- prohibit nulls from being stored in tables if verbii used nil==null. so to make
-- the code cleaner, nil is reserved for Lua-specific things and never means verbii null.
local Null = {}
function Null:new(obj, value)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "Null"
	return obj
end

function isNull(obj)
	return type(obj) == "table" and obj.__class__ == "Null"
end

function new_Null()
	return Null:new({})
end

-- as with other ports, need a Void that is differentiated from Null. 
-- again, I'm not using nil since it should be *possible* to store
-- void in data structures (although this is normally not done)
local Void = {}
function Void:new(obj, value)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "Void"
	return obj
end

function isVoid(obj)
	return type(obj) == "table" and obj.__class__ == "Void"
end

function new_Void()
	return Void:new({})
end

function isBool(obj)
	return type(obj) == "boolean"
end

-- lua doesn't differentiate between int and float, so need a class ...
local Float = {}
-- next 3 are meant to be global
FLOAT_PRECISION = 17
-- see c++ notes on limits
MAX_VINT = (1<<53) - 1
MIN_VINT = -MAX_VINT

function isInt(obj)
	return type(obj) == "number"
end

function Float:new(obj, value)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "Float"
	obj.value = value
	return obj
end

function isFloat(obj)
	return type(obj) == "table" and obj.__class__ == "Float"
end

function new_Float(value)
	return Float:new({}, value)
end

function isNumeric(obj)
	return isInt(obj) or isFloat(obj)
end

function asNumeric(obj)
	-- this always returns a float
	if isInt(obj) then
		-- this seems to make it act like a float ... without this there will be large
		-- errors on math unittests
		return obj*1.0
	elseif isFloat(obj) then
		return obj.value
	else
		error("asNumeric() got non-numeric object: " .. fmtStackPrint(obj))
	end
end

function parseBool(text)
	if text == "true" then return true
	elseif text == "false" then return false
	else error(">>>Bad boolean literal: " .. text)
	end
end
	
-- as in the Python port, regular Lua strings as used as symbols,
-- and strings get their own type
local String = {}

function String:new(obj, str)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "String"
	obj.value = str
	return obj
end

function isString(obj)
	return type(obj) == "table" and obj.__class__ == "String"
end

function new_String(str)
	return String:new({}, str)
end

-- dictionary type
local Dict = {}
function Dict:new(obj)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "Dict"
	obj.dict = {}
	return obj
end

function new_Dict()
	return Dict:new({})
end

function isDict(obj)
	return type(obj) == "table" and obj.__class__ == "Dict"
end

-- lambda type
local Lambda = {}
function Lambda:new(obj, objlist)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "Lambda"
	obj.objlist = objlist
	return obj
end

function new_Lambda(objlist)
	return Lambda:new({},objlist)
end

function isLambda(obj)
	return type(obj) == "table" and obj.__class__ == "Lambda"
end

-- closure
local Closure = {}
function Closure:new(obj, objlist, state)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "Closure"
	obj.objlist = objlist
	obj.state = state
	return obj
end

function new_Closure(objlist, state)
	return Closure:new({},objlist,state)
end

function isClosure(obj)
	return type(obj) == "table" and obj.__class__ == "Closure"
end

-- normal lua strings are used as symbols
function isSymbol(obj)
	return type(obj) == "string"
end

-- a table with no __class__ is a list
function isList(obj)
	return type(obj) == "table" and obj.__class__ == nil
end

function fmtDisplayObjlist(objlist,open_delim,close_delim)
	local s = open_delim
	for i=1,#objlist do
		s = s .. " " .. fmtDisplay(objlist[i])
	end
	s = s .. " " .. close_delim
	return s
end

-- get keys from a Dict object
function dictKeys(obj)
	local keys = {}
	for k,v in pairs(obj.dict) do
		table.insert(keys,k)
	end
	return keys
end

-- as above but sort the keys
function sortedDictKeys(obj)
	local keys = dictKeys(obj)
	table.sort(keys)
	return keys
end

-- slow but i don't know a better way to do this .. only call when really needed
function dictSize(obj)
	local count = 0
	for k,v in pairs(obj.dict) do
		count = count + 1
	end
	return count
end

-- see c++ comments for display vs. stack format
function fmtDisplay(obj)
	if isVoid(obj) then
		return "<*void*>"
	elseif isInt(obj) then
		return tostring(obj)
	elseif isNull(obj) then
		return "<null>"
	elseif isFloat(obj) then
		local fmt = "%." .. tostring(FLOAT_PRECISION) .. "g"
		return string.format(fmt, obj.value)
	elseif isBool(obj) then
		if obj then
			return "true"
		else
			return "false"
		end
	elseif isLambda(obj) then
		return "<" .. fmtDisplayObjlist(obj.objlist,"{","}") .. ">"
	elseif isString(obj) then
		return obj.value
	elseif isSymbol(obj) then
		return obj
	elseif isList(obj) then
		return fmtDisplayObjlist(obj, "[", "]")
	elseif isDict(obj) then
		local s = "{ "
		local keys = sortedDictKeys(obj)
		for i=1,#keys do
			s = s .. "\"" .. keys[i] .. "\" => " .. fmtDisplay(obj.dict[keys[i]]) .. " "
		end
		s = s .. "}"
		return s
	elseif isClosure(obj) then
		return "<" .. fmtDisplayObjlist(obj.objlist,"{","}") .. " :: " ..
				fmtDisplay(obj.state) .. ">"
	else
		error(">>>Don't know how to print object: " .. tostring(obj))
	end
end

function fmtStackPrintObjlist(objlist,open_delim,close_delim)
	local s = open_delim
	for i=1,#objlist do
		s = s .. " " .. fmtStackPrint(objlist[i])
	end
	s = s .. " " .. close_delim
	return s
end


-- see c++ comments for display vs. stack format
function fmtStackPrint(obj)
	if isVoid(obj) then
		return "<*void*>" -- the type that means "nothing, not even null"
	elseif isNull(obj) then
		return "<null>"
	elseif isInt(obj) then
		return tostring(obj)
	elseif isFloat(obj) then
		local fmt = "%." .. tostring(FLOAT_PRECISION) .. "g"
		return "#" .. string.format(fmt, obj.value)
	elseif isBool(obj) then
		if obj then
			return "<true>"
		else
			return "<false>"
		end
	elseif isLambda(obj) then
		return "<" .. fmtStackPrintObjlist(obj.objlist,"{","}") .. ">"
	elseif isDict(obj) then
		local s = "{ "
		local keys = sortedDictKeys(obj)
		for i=1,#keys do
			s = s .. "\"" .. keys[i] .. "\" => " .. fmtStackPrint(obj.dict[keys[i]]) .. " "
		end
		s = s .. "}"
		return s
	elseif isClosure(obj) then
		return "<" .. fmtStackPrintObjlist(obj.objlist,"{","}") .. " :: " ..
				fmtStackPrint(obj.state) .. ">"
	elseif isString(obj) then
		-- in stack display, strings get " .. "
		return '"' .. obj.value .. '"'
	elseif isSymbol(obj) then
		return "'" .. obj
	elseif isList(obj) then
		return fmtStackPrintObjlist(obj, "[", "]")
	else
		error(">>>Don't know how to print object: " .. tostring(obj))
	end
end

-- see c++ & DESIGN-NOTES.md
function deepcopyObjlist(objlist)
	local newlist = {}
	for i=1,#objlist do
		table.insert(newlist, deepcopy(objlist[i]))
	end
	return newlist
end

function deepcopy(obj)
	if isNull(obj) or isInt(obj) or isFloat(obj) or
		isBool(obj) or isLambda(obj) or isClosure(obj) or
		isString(obj) or isSymbol(obj) or isVoid(obj) then
		return obj
	elseif isList(obj) then
		return deepcopyObjlist(obj)
	elseif isDict(obj) then
		local ndict = {}
		for k,v in pairs(obj.dict) do
			ndict[k] = deepcopy(v)
		end
		return ndict
	else
		error(">>>Don't know how to deepcopy object: " .. tostring(obj))
	end
end
