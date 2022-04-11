--[[
	Deserialize - load bytecode from compiler and put into Interpreter.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the Python implementation.
]]--

require("interpreter")

function deserialize_stream(intr, fileIn)
	local line = fileIn:read('l')
	if line == fail then
		return nil
	end
	--print("LINE: " .. line)
	if line:sub(1,1) == "i" then
		return tonumber(line:sub(3))
	elseif line:sub(1,1) == "f" then
		return new_Float(tonumber(line:sub(3)))
	elseif line:sub(1,1) == "n" then
		return new_None()
	elseif line:sub(1,1) == "b" then
		if line:sub(3) == "true" then
			return true
		else
			return false
		end
	elseif line:sub(1,1) == "s" then
		local s = line:sub(3)
		-- unescape string
		s = s:gsub("%%32", " ")
		s = s:gsub("%%09", "\t")
		s = s:gsub("%%10", "\n")
		s = s:gsub("%%13", "\r")
		s = s:gsub("%%37", "%%")
		return new_String(s)
	elseif line:sub(1,1) == "y" then
		return line:sub(3) -- symbols don't need escaping
	elseif line:sub(1,1) == "L" then
		local nr = tonumber(line:sub(3))
		local objs = {}
		for i=1,nr do
			objs[i] = deserialize_stream(intr,fileIn)
		end
		return objs
	elseif line:sub(1,1) == "F" then --lambda
		local objs = deserialize_stream(intr, fileIn)
		if isList(objs) then
			return new_Lambda(objs)
		else
			error(">>>Expecting list after 'F' but got: " .. fmtStackPrint(objs))
		end
	elseif line:sub(1,1) == "W" then
		local name = line:sub(3)
		local objs = deserialize_stream(intr, fileIn)
		if isList(objs) then
			intr:defineWord(name, objs, false) -- do not allow overwriting existing words
		else
			error(">>>Expecting list after 'W' but got: " .. fmtStackPrint(objs))
		end
	else
		error(">>>Unrecognized line while deserializing: " .. line)
	end
end


		
	
		
		
		
	
		
