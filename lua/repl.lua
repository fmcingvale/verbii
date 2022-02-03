--[[
	repl - run code interactively, run unittests, or run programs.

	Copyright (c) 2022 Frank McIngvale, see LICENSE

	Ported from Python implementation
]]

require("interpreter")

function repl()
	-- Run interactively
	intr = new_Interpreter()

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

repl()
