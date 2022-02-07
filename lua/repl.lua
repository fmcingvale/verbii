--[[
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from Python implementation
]]

require("interpreter")

INITLIB = "../lib/init.txt"

function make_interpreter(noinit)
	-- convenience to start interpreter and optionally load init lib
	intr = new_Interpreter()
	
	if not noinit then
		-- run initlib to load its words first
		f = io.open(INITLIB, "r")
		buf = f:read("a")
		intr:addText(buf)
		io.close(f)
		intr:run()
		-- don't want initlib in the backtrace history, once it has successfully loaded
		intr.reader:clearAll()
	end

	return intr
end

function repl()
	-- Run interactively
	intr = make_interpreter(false)

	while true do
		io.write(">> ")
		io.flush()
		line = io.read()
		if line == nil then
			return -- eof
		elseif line == "quit" then
			return
		end
		
		intr:addText(line)
		intr:run()
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
		intr.reader:clearAll() -- remove any leftover text from previous line run
		intr:addText(line)
		intr:run()
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
	f = io.open(filename, "r")
	buf = f:read("a")
	io.close(f)
	intr:addText(buf)
	if singlestep then
		intr:run(debug_hook)
	else
		intr:run()
	end
end

filename = nil
noinit = false
test_mode = false
singlestep = false

for i=1,#arg do
	if arg[i] == "-noinit" then
		noinit = true
	elseif arg[i] == "-test" then
		test_mode = true
	elseif arg[i] == "-step" then
		singlestep = true
	elseif filename == nil then
		filename = arg[i]
	else
		error(">>>Bad command line argument: " .. arg[i])
	end
end

if filename == nil then
	repl()
elseif test_mode then
	status = {}
	while not status["done"] do
		local result,error = pcall(run_test_mode, filename, noinit, status)
		--print("RESULT:",result)
		if result == false then
			-- match sequence >>> to strip out filename from error message that lua added
			match = string.match(error, "^.+>>>")
			print("*** " .. string.sub(error, #match+1) .. " ***")
			--print("MAX COUNT " .. tostring(status["max-count"]))
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
		print("*** " .. string.sub(error, #match+1) .. " ***")
		print_backtrace(intr)
	end
end

