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
		if line == "quit" then
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
	intr = make_interpreter(noinit)

	if status["max-count"] == nil then status["max-count"] = 0 end
	status["done"] = false

	fileIn = io.open(filename,"r")
	runnable_lines = 0 -- how many lines have I seen that are non-blank
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
			status["max-count"] = max(status["max-count"],runnable_lines)
			goto LOOP
		end
		
		io.write(">> " .. line) -- line has \n at end already
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

filename = nil
noinit = false

for i=1,#arg do
	if arg[i] == "-noinit" then
		noinit = true
	elseif filename == nil then
		filename = arg[i]
	else
		error("Bad command line argument: " .. arg[i])
	end
end

if filename == nil then
	repl()
else
	run_test_mode(filename, noinit, {})
end

