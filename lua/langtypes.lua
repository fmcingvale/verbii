--[[
	LangTypes - types that can't be represented with native Lua types.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Complete typemappings:

	verbii		Lua
	------		---
	null		None (class)
	int			number
	float		Float (class)
	boolean		boolean
	symbol		string	** since symbols are more commonly used in the Lua source, then
						** like Python, use Lua strings for symbols
	string		String (class)
	lambda		Lambda (class)
	list		table (i.e. plain table with no __class attribute)
--]]

-- make a "none" class that is differentiated from nil
local None = {}
function None:new(obj, value)
	setmetatable(obj, self)
	self.__index = self
	obj.__class__ = "None"
	return obj
end

function isNone(obj)
	return type(obj) == "table" and obj.__class__ == "None"
end

function new_None()
	return None:new({})
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

function isLambda(obj)
	return type(obj) == "table" and obj.__class__ == "Lambda"
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

-- see c++ comments for display vs. stack format
function fmtDisplay(obj)
	if type(obj) == "number" then
		return tostring(obj)
	elseif isNone(obj) then
		return "<null>"
	elseif isFloat(obj) then
		local fmt = "%." .. tostring(FLOAT_PRECISION) .. "g"
		return string.format(fmt, obj.value)
	elseif type(obj) == "boolean" then
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
	if obj == nil then
		return "<VOID>" -- the type that means "nothing, not even null"
	elseif isNone(obj) then
		return "<null>"
	elseif type(obj) == "number" then
		return tostring(obj)
	elseif isFloat(obj) then
		local fmt = "%." .. tostring(FLOAT_PRECISION) .. "g"
		return "#" .. string.format(fmt, obj.value)
	elseif type(obj) == "boolean" then
		if obj then
			return "<true>"
		else
			return "<false>"
		end
	elseif isLambda(obj) then
		return "<" .. fmtStackPrintObjlist(obj.objlist,"{","}") .. ">"
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
	if obj == nil or isNone(obj) or type(obj) == "number" or isFloat(obj) or
		type(obj) == "boolean" or isLambda(obj) or isClosure(obj) or
		isString(obj) or isSymbol(obj) then
		return obj
	elseif isList(obj) then
		return deepcopyObjlist(obj)
	else
		error(">>>Don't know how to deepcopy object: " .. tostring(obj))
	end
end
