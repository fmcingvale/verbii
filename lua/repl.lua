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

function make_interpreter()
	local intr = new_Interpreter()
	
	-- load bootstrap libraries so compiler works
	local f = io.open(INITLIB, "r")
	deserialize_stream(intr, f)
	io.close(f)
	local code = intr.WORDS['__main__']
	intr:run(code)

	f = io.open(COMPILERLIB, "r")
	deserialize_stream(intr, f)
	io.close(f)
	-- do NOT run __main__ since that would run the cmdline compiler

	-- remove __main__ so i don't try to run it again later
	intr.WORDS['__main__'] = nil
	
	return intr
end

function repl()
	-- Run interactively
	intr = make_interpreter()

	while true do
		io.write(">> ")
		io.flush()
		line = io.read()
		if line == nil then
			return -- eof
		elseif line == "quit" then
			return
		end
		
		-- push line and byte-compile it
		intr:push(new_String(line))
		code = intr.WORDS['byte-compile-string']
		intr:run(code)

		-- remove list of words produced by byte compiler
		intr:pop()

		-- now run the __main__ that was compiled
		code = intr.WORDS['__main__']
		intr:run(code)
		
		print("=> " .. intr:reprStack())
	end
end

function run_test_mode(filename, noinit, status)
	-- read one line at a time from file and run, printing results and stack. 
	-- used for unit testing
	local intr = make_interpreter(noinit)

	if status["max-count"] == nil then status["max-count"] = 0 end
	status["done"] = false

	local fileIn = io.open(filename,"r")
	local runnable_lines = 0 -- how many lines have I seen that are non-blank
	while true do
		::LOOP::
		line = fileIn:read("L")
		if not line then
			break
		end
		if string.match(line, "^[%s]+$") then
			goto LOOP -- don't count blank lines
		end

		runnable_lines = runnable_lines + 1
		
		if runnable_lines <= status["max-count"] then
			-- skipping line counts as running since either i successfully ran
			-- it previously, or am skipping it because it crashed
			status["max-count"] = math.max(status["max-count"],runnable_lines)
			goto LOOP
		end
		
		io.write(">> " .. line) -- line has \n at end already
		--intr.syntax:clearAll() -- remove any leftover text from previous line run

		-- push line and byte-compile it
		intr:push(new_String(line))
		local code = intr.WORDS['byte-compile-string']
		intr:run(code)

		-- remove list of words produced by byte compiler
		intr:pop()

		-- now run the __main__ that was compiled
		code = intr.WORDS['__main__']
		intr:run(code)
		
		print("=> " .. intr:reprStack())
		-- update count AFTER above suceeds
		status["max-count"] = runnable_lines
	end
	io.close(fileIn)
	-- made it all the way through, set 'done'
	status["done"] = True
end

function backtrace_curframe(intr)
	local trace = ""
	local nr = 7 -- number of words to print in each frame
	while nr > 0 do
		w = intr.reader:prevWord()
		if w == "" then
			print(trace)
			return
		else
			trace = w .. " " .. trace
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
		if intr.reader:hasPushedWords() then
			intr.reader:popWords()
		else
			return
		end
	end
end

function debug_hook(intr, word)
	print("=> " .. intr:reprStack())
	print("Run: " .. word)
	io.write("press ENTER to continue ...")
	io.flush()
	io.read()
end

function run_file(intr, filename, singlestep)
	-- run file
	local f = io.open(filename, "r")
	if f == nil then
		error(">>>No such file: " .. filename)
	end
	local buf = f:read("a")
	io.close(f)

	-- push buf and byte-compile it
	intr:push(new_String(buf))
	local code = intr.WORDS['byte-compile-string']
	intr:run(code)

	-- remove list of words produced by byte compiler
	intr:pop()

	-- now run the __main__ that was compiled
	code = intr.WORDS['__main__']

	if singlestep then
		intr:run(code,debug_hook)
	else
		intr:run(code)
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
	repl(noinit)
elseif test_mode then
	status = {}
	while not status["done"] do
		--run_test_mode(filename, noinit, status)
		local result,error = pcall(run_test_mode, filename, noinit, status)
		--print("RESULT:",result)
		if result == false then
			-- match sequence >>> to strip out filename from error message that lua added
			match = string.match(error, "^.+>>>")
			if match == nil then
				-- didn't get expected error format, so print raw error to show something at least
				print("*** " .. error .. " ***")
				os.exit() -- just exit since something is extra wrong here ...
			else
				print("*** " .. string.sub(error, #match+1) .. " ***")
				--print("MAX COUNT " .. tostring(status["max-count"]))
			end
			status["max-count"] = status["max-count"] + 1
		elseif result==true then
			break
		end
	end
else
	local intr = make_interpreter(noinit)
	local result,error = pcall(run_file, intr, filename, singlestep)
	if result == false then
		-- match sequence >>> to strip out filename from error message that lua added
		match = string.match(error, "^.+>>>")
		-- like above, if that didn't match, print raw error and exit
		if match == nil then
			print("*** " .. error .. " ***")
			os.exit()
		else
			print("*** " .. string.sub(error, #match+1) .. " ***")
			print_backtrace(intr)
		end
	end
end

