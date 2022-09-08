--[[
	Deserialize - load bytecode from compiler and put into Interpreter.
	
	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from the Python implementation.
]]--

require("interpreter")

function trimCRLF(text)
	while text:sub(#text) == "\n" or text:sub(#text) == "\r" do
		text = text:sub(1,#text-1)
	end
	
	return text
end

function deserialize_stream(intr, fileIn)
	local line = fileIn:read('l')
	if line == fail then
		return new_Void()
	end
	-- get rid of extra CR/LF -- could have been written by another platform so the
	-- above :read('l') didn't remove the entire line ending
	line = trimCRLF(line)
	--print("LINE: " .. line)
	if line:sub(1,1) == "M" then
		-- ignore metadata and return NEXT object 
		return deserialize_stream(intr, fileIn)
	elseif line:sub(1,1) == "i" then
		return tonumber(line:sub(3))
	elseif line:sub(1,1) == "f" then
		return new_Float(tonumber(line:sub(3)))
	elseif line:sub(1,1) == "b" then
		return parseBool(line:sub(3))
	elseif line:sub(1,1) == "n" then
		return new_Null()
	elseif line:sub(1,1) == "o" then
		return opcode_from_packed(tonumber(line:sub(3)))
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


		
	
		
		
		
	
		
