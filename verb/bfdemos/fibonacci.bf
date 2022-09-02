A probably very unoptimized Fibonacci generator, by me (Frank McIngvale).
Print first N fibonacci numbers (set N on first line of code).
!
++++++++++++++++++			# M0 = number to generate
>++++++++++ 				# M1 = linefeed
>>+							# M2=0 M3=1 -- previous 2 numbers in sequence

backup to M0 loop counter to begin
<<<
[
# move to M3 and print it
>>>

	# print routine from: https://bit.ly/3CHPreY
	# * print number at DP, zeroes out DP+1, ...
	>[-]>[-]+>[-]+<[>[-<-<<[->+>+<<]>[-<+>]>>]++++++++++>[-]+>[-]>[-]>[-]<<<<<[->-[>+>>]>[[-<+>]+>+>>]<<<<<]
	>>-[-<<+>>]<[-]++++++++[-<++++++>]>>[-<<+>>]<<]<[.[-]<]< # leave DP at M3
	
	# print linefeed
	<<.>>
	
# next steps: 	M4 = M2+M3 (will become 2nd number)
#				M5 = M3 (2nd number; will become 1st number)
zero M4 before adding M3
>[-]

need to save M3 since it will become M2 so make 2 copies into M4 and M5
<				DP=M3
[				while M3!=0
>+>+			inc M4 M5
<<-				dec M3
]

add M2 to M4 to complete sum of M2 and M3
<				DP=M2
[				while M2
>>+				inc M4
<<-				dec M2
]

move M5 (old 2nd number) to M2
>>>				DP=M5
[
	<<<+>>>-
]

# move M4 to M3 (sum -> new 2nd number)
<				DP=M4
[				# while M4 != 0
	<+>-		# ++M3 --M4
]


move back to M0 decr then loop
<<<<-
]

?


