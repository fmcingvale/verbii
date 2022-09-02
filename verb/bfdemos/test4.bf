Print A..Z but not from input
!
set M0=65

++++++			M0=6
[				while M0!=0
	>
	++++++++++	M1 = 10
	[			while M1!=0
		>+		inc M2
		<-		dec M1
	]
	<-			dec M0
]				M2=60
>>+++++			M2=65

set M3=26 for # number of letters in alphabet
<<++++			M0=4
[				while M0!=0
	>+++++		M1=5
	[			while M1!=0
		>>+		inc M3
		<<-		dec M1
	]
	<-			dec M0
]
>>>++++++		M3=26
[				while M3!=0
	<.+>-		print M2 inc M2 dec M3
]
	
?
