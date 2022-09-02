algorithms from: https://bit.ly/3CHPreY
!
	# x=y (without disturbing y)
	# x = M2; y = M3; temp = M4
	# set y=12 as a test
	>>++++++++++++	# DP=M3 (y)
	>[-] 			# temp0[-]
	<<[-]			# x[-]
	>[<+>>+<-]		# y[x+temp0+y-]
	>[<+>-]			# temp0[y+temp0-]
	?
	[-]<[-]<[-]		# clear M2,M3,M4
	?

	# x = x+y (without disturbing y)
	# x=M2, y=M2, temp=M3
	# set x=10
	++++++++++
	# set y=12
	>++++++++++++
	# code
	>[-]			# temp0[-]
	<[<+>>+<-]	 	# y[x+temp0+y-]
	>[<+>-]			# temp0[y+temp0-]
	?
	[-]<[-]<[-]		# clear M2,M3,M4
	?
	
	# x = x-y (without disturbing y)
	# x=M2, y=M3, temp=M4
	# set x=5
	+++++
	# set y=8
	>++++++++
	# code
	>[-]			# temp0[-]
	<[<->>+<-]		# y[x-temp0+y-]
	>[<+>-]			# temp0[y+temp0-]
	?
	<<+++
	?
	
	>>[-]<[-]<[-]	# clear M2,M3,M4
	?
	
	# x = x*y (without disturbing y)
	# x=M2, y=M3, temp0=M4, temp1=M5
	# set x=12
	++++++++++++
	# set y=8
	>++++++++
	?
	# code
	>[-]				# temp0[-]
	>[-]				# temp1[-]
	<<<[>>>+<<<-]		# x[temp1+x-]
	>>>[				# temp1[
	<<[<+>>+<-]>[<+>-]	# y[x+temp0+y-]temp0[y+temp0-]
	>-]					# temp1-]
	?
	
	[-]<[-]<[-]<[-]		# clear M5 .. M2
	?
	
	# Print value (minimized)
	# Cells used: V Z n d 1 0 0 0
	# V is the value you need to print; it is not modified
	# Z is a zero sentinal and tmp
	# All cells Z and up are cleared by this routine

	# set M1 (V) to 36
	++++++++++++
	++++++++++++
	++++++++++++
	?
	
	>[-]>[-]+>[-]+<[>[-<-<<[->+>+<<]>[-<+>]>>]++++++++++>[-]+>[-]>[-]>[-]<<<<<[->-[>+>>]>[[-<+>]+>+>>]<<<<<]
	>>-[-<<+>>]<[-]++++++++[-<++++++>]>>[-<<+>>]<<]<[.[-]<]< # leave DP at V
	?
		
	
	
	