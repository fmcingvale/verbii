--[[
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from Python implementation
]]

require("deserialize")
require("interpreter")
require("langtypes")
require("native")

INITLIB = "../lib/init.verb.b"
COMPILERLIB = "../lib/compiler.verb.b"
PATCHESLIB = "../lib/patches.verb"

SHOW_RUNTIME_STATS = false

function readfile(filename)
	local f = io.open(filename, "r")
	if f == nil then
		error(">>>No such file: " .. filename)
	end
	local buf = f:read("a")
	io.close(f)
	return buf
end

function compile_and_load(intr, text, allow_overwrite)
	ALLOW_OVERWRITING_WORDS = allow_overwrite
	intr:push(new_String(text))
	code = intr:lookupWordOrFail('compile-and-load-string')
	intr:run(code)
	-- turn flag back off (default)
	ALLOW_OVERWRITING_WORDS = false
end

function deserialize_and_run(intr, filename)
	local f = io.open(filename, "r")
	deserialize_stream(intr, f)
	io.close(f)
	local code = intr:lookupWordOrFail('__main__')
	intr:run(code)
	-- always delete __main__ after running, or next file will fail to load
	intr:deleteWord('__main__')
end

function debug_hook(intr, word)
	print("=> " .. intr:reprStack())
	print("Run: " .. word)
	io.write("press ENTER to continue ...")
	io.flush()
	io.read()
end

-- use safe_ version instead - this has no error checking
function compile_and_run(intr, text, singlestep, allow_overwrite)
	if allow_overwrite == null then allow_overwrite = false end

	compile_and_load(intr, text, allow_overwrite)
	
	-- now run the __main__ that was compiled
	code = intr:lookupWordOrFail('__main__')
	
	if singlestep then
		intr:run(code,debug_hook)
	else
		intr:run(code)
	end
	
	-- remove __main__ when done
	intr:deleteWord('__main__')
end

function make_interpreter(verbose)
	local intr = new_Interpreter()
	
	-- load bootstrap libraries so compiler works
	deserialize_and_run(intr, INITLIB)
	deserialize_and_run(intr, COMPILERLIB)
	
	-- load & run patches -- allow patches to overwrite existing words
	local buf = readfile(PATCHESLIB)
	-- if this will take a bit, print a message
	if verbose and #buf > 1000 then print("Patching ...") end
	compile_and_run(intr, readfile(PATCHESLIB), false, true)

	return intr
end

-- returns error string or nil if no error
function safe_compile_and_run(intr, text, singlestep, backtrace_on_error)
	local result,error = pcall(compile_and_run, intr, text, singlestep, false)
	if result == false then
		-- match sequence >>> to strip out filename from error message that lua added
		match = string.match(error, "^.+>>>")
		if match == nil then
			-- didn't get expected error format, so print raw error to show something at least
			errstr = "*** " .. error .. " ***"
			return errstr
		else
			errstr = "*** " .. string.sub(error, #match+1) .. " ***"
			if backtrace_on_error then
				print_backtrace(intr)
			end
			return errstr
		end
	else
		return nil -- no error
	end
end

function repl(singlestep)
	-- Run interactively
	print("Verbii running on " .. _VERSION)
	intr = make_interpreter(true)

	while true do
		io.write(">> ")
		io.flush()
		line = io.read()
		if line == nil then
			return -- eof
		elseif line == "quit" or line == ",q" then
			if SHOW_RUNTIME_STATS then
				intr:print_stats()
			end
			return
		end
		
		-- compile & run each line, watching for errors
		local err = safe_compile_and_run(intr, line, singlestep, true)
		if err ~= nil then
			print(err)
			intr = make_interpreter() -- restart interpreter on error
		else
			print("=> " .. intr:reprStack())
		end
	end
end

function run_test_mode(filename)
	-- read one line at a time from file and run, printing results and stack. 
	-- used for unit testing
	local intr = make_interpreter(noinit)

	local fileIn = io.open(filename,"r")
	while true do
		::LOOP::
		line = fileIn:read("l") -- discard \n at end of line
		if not line then
			break
		end
		if string.match(line, "^[%s]*$") then
			goto LOOP -- skip blank lines
		end

		print(">> " .. line)
		
		-- i only want the errors not the backtraces -- if an unexpected error occurred,
		-- just run again in non-test mode to see backtrace
		local err = safe_compile_and_run(intr, line, false, false)
		if err ~= nil then
			print(err)
			intr = make_interpreter() -- restart interpreter on error
		else
			print("=> " .. intr:reprStack())
		end
	end
	io.close(fileIn)
end

function backtrace_curframe(intr)
	local trace = ""
	local nr = 7 -- number of words to print in each frame
	while nr > 0 do
		local w = intr:prevObj()
		if w == nil then
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

function run_file(intr, filename, singlestep)
	-- load & run file
	local buf = readfile(filename)
	local err = safe_compile_and_run(intr, buf, singlestep, true)
	if err ~= nil then
		print(err)
	elseif SHOW_RUNTIME_STATS then
		intr:print_stats()
	end
end

filename = nil
noinit = false
test_mode = false
singlestep = false
args_to_script = {}

for i=1,#arg do
	if arg[i] == "-noinit" then
		noinit = true
	elseif arg[i] == "-test" then
		test_mode = true
	elseif arg[i] == "-step" then
		singlestep = true
	elseif arg[i] == "-stats" then
		SHOW_RUNTIME_STATS = true
	elseif arg[i] == "--" then
		-- rest of args go to script
		i = i + 1
		while i <= #arg do
			table.insert(args_to_script, new_String(arg[i]))
			i = i + 1
		end
		break
	elseif filename == nil then
		filename = arg[i]
	else
		error(">>>Bad command line argument: " .. arg[i])
	end
end

set_native_cmdline_args(args_to_script)

if filename == nil then
	repl(singlestep)
elseif test_mode then
	run_test_mode(filename)
else
	local intr = make_interpreter(noinit)
	run_file(intr, filename, singlestep)
end
