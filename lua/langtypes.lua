--[[
	LangTypes - types that can't be represented with native Lua types.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
--]]

-- lua doesn't differentiate between int and float, so need a class ...
Float = {}
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

function isCallableWordlist(obj)
	return type(obj) == "table" and obj.__class__ == "CallableWordlist"
end

-- lambda type
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

function reprObject(obj)
	if type(obj) == "number" then
		return tostring(obj)
	elseif isFloat(obj) then
		return string.format("%.17f", obj.value)
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

