--[[
	test_reader - informal tests of the Reader. Lua doesn't seem to have a standard
	unittest package so I just did this ad-hoc. As noted in the C++ and Python
	implementations, I'm not big on writing unittests for this, just enough to
	check the Reader since it's hard to test within the language.

	Copyright (c) 2022 Frank McIngvale, see LICENSE
]]
require("reader")

s = "  one  two   three   four  ; : 123\r\n456 .\"  hello \"   \t "

reader = new_Reader()
reader:addText(s)
while 1 do
	w = reader:nextWord()
	if w == "" then
		break
	else
		print(w)
	end
end
print("PREV:")
while 1 do
	w = reader:prevWord()
	if w == "" then
		break
	else
		print(w)
	end
end
print("Each twice:")
while 1 do
	w = reader:peekWord()
	if w == "" then
		break
	end
	print(w)
	w = reader:nextWord()
	print(w)
end
print("PUSH")
reader:prevWord()
reader:prevWord()
reader:prevWord()
reader:prevWord()
reader:prevWord()
print("Next from original will be:",reader:peekWord())
print("PUSHING")
reader:pushWords({'aaa','bbb','ccc'})
print("AFTER PUSH")
print(reader:nextWord())
print("OOOOOOO")
print(reader:nextWord())
print(reader:nextWord())
if reader:hasPushedWords() then
	reader:popWords()
	print(reader:nextWord())
	print(reader:nextWord())
	print(reader:nextWord())
end

reader:clearAll()
reader:addText("   111 222 333 444  ")
print(reader:nextWord())
print(reader:nextWord())
print(reader:nextWord())
reader:deletePrevWord()
reader:deletePrevWord()
reader:insertPrevWord("new-1")
reader:insertPrevWord("new-2")
reader:insertPrevWord("new-3")
print("Expect 444")
print(reader:nextWord())
while 1 do
	w = reader:prevWord()
	if w == "" then
		break
	end
end
print("Expect 111 new-1 new-2 new-3 444")

while 1 do
	w = reader:nextWord()
	if w == "" then
		break
	end
	print(w)
end
