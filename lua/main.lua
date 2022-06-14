--[[
	Verbii frontend -- only loads & runs boot.verb.b

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from Python implementation
]]

require("deserialize")
require("interpreter")
require("langtypes")
require("native")

local BOOTFILE = "../lib/boot.verb.b"

local SHOW_RUNTIME_STATS = false

function backtrace_curframe(intr)
	local trace = ""
	local nr = 7 -- number of words to print in each frame
	while nr > 0 do
		local w = intr:prevObj()
		if isVoid(w) then
			print(trace)
			return
		else
			trace = fmtStackPrint(w) .. " " .. trace
		end
		nr = nr - 1
	end
	
	print(trace)
end

function print_backtrace(intr)
	local i=0
	while true do
		io.write("FRAME " .. tostring(i) .. ": ")
		i = i + 1
		backtrace_curframe(intr)
		if intr:havePrevFrames() then
			intr:code_return() -- pop current frame and print next one
		else
			return -- end of callstack, done
		end
	end
end

function deserialize_and_run(intr, filename)
	local f = io.open(filename, "r")
	deserialize_stream(intr, f)
	io.close(f)
	local code = intr:lookupWordOrFail('__main__')
	-- delete __main__ *before* running code so code can redefine __main__ if desired
	intr:deleteWord('__main__')
	intr:run(code)
	
end

local args_to_script = {}

for i=1,#arg do
	if arg[i] == "-stats" then
		SHOW_RUNTIME_STATS = true
	elseif arg[i] == "--" then
		-- rest of args go to script
		i = i + 1
		while i <= #arg do
			table.insert(args_to_script, new_String(arg[i]))
			i = i + 1
		end
		break
	else
		-- unknown args go to script
		table.insert(args_to_script, new_String(arg[i]))
	end
end

set_native_cmdline_args(args_to_script)

local intr = nil
while true do
	intr = new_Interpreter()
	local result,error = pcall(deserialize_and_run, intr, BOOTFILE)
	if result == false then
		-- match sequence >>> to strip out filename from error message that lua added
		local match = string.match(error, "^.+>>>")
		if match == nil then
			-- didn't get expected error format, so print raw error to show something at least
			local errstr = "*** " .. error .. " ***"
			print(errstr)
			if EXIT_ON_EXCEPTION then
				os.exit(1)
			end
		else
			local errstr = "*** " .. string.sub(error, #match+1) .. " ***"
			if STACKTRACE_ON_EXCEPTION then
				print_backtrace(intr)
			end
			print(errstr)
			if EXIT_ON_EXCEPTION then
				os.exit(1)
			end
		end
	else
		break -- ran successfully, end
	end
end
