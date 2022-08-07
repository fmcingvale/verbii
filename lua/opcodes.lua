--[[
	Opcodes

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from Python version
]]

-- C++ is the reference -- see docs there

-- these values must match the C++ implementation
OPCODE_FRAME_GET = 0
OPCODE_FRAME_SET = 1

--- opcode functions ---

function opcode_FRAME_GET(intr, levels, index, _unused)
	if intr.framedata == nil then
		error(">>>opcode FRAME-GET called on null frame")
	end

	intr:push(intr.framedata:getFrameObj(levels, index))
end

function opcode_FRAME_SET(intr, levels, index, _unused)
	if intr.framedata == nil then
		error(">>>opcode FRAME-SET called on null frame")
	end

	intr.framedata:setFrameObj(levels, index, intr:pop())
end
	
-- this is a list instead of dict for performance -- make sure ordering
-- matches the constants order
OPCODE_FUNCTIONS = {
	opcode_FRAME_GET, opcode_FRAME_SET
}

-- these are not speed critical so can use maps so these are order-independent
local NAME_TO_CODE = {}
NAME_TO_CODE["FRAME-GET"] = OPCODE_FRAME_GET
NAME_TO_CODE["FRAME-SET"] = OPCODE_FRAME_SET

local CODE_TO_NAME = {}
CODE_TO_NAME[OPCODE_FRAME_GET] = "FRAME-GET"
CODE_TO_NAME[OPCODE_FRAME_SET] = "FRAME-SET"

function opcode_name_to_code(name)
	if NAME_TO_CODE[name] == nil then
		error(">>>No such opcode name: " .. name)
	end

	return NAME_TO_CODE[name]
end

function opcode_code_to_name(code)
	if CODE_TO_NAME[code] == nil then
		error(">>>No such opcode number: " .. tostring(code))
	end

	return CODE_TO_NAME[code]
end


