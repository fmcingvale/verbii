M v1:1106413674
L 71
W reader-open-string
L 8
o 1
o 0
y READER-TEXT!
y READER-TEXT
y length
y READER-TEXT-LEN!
i 0
y READER-POS!
W reader-open-file
L 4
o 1
o 0
y file-read
y reader-open-string
W reader-peek-char
L 13
y READER-POS
y READER-TEXT-LEN
y >=
y not
y if
o 12884901890
y void
o 21474836482
y @auto-symbol-2
y READER-TEXT
y READER-POS
y get
y @auto-symbol-1
W reader-next-char
L 17
y READER-POS
y READER-TEXT-LEN
y >=
y not
y if
o 12884901890
y void
o 38654705666
y @auto-symbol-4
y READER-TEXT
y READER-POS
y get
y READER-POS
i 1
y +
y READER-POS!
y @auto-symbol-3
W reader-next-chunk
L 74
s 
o 1
y reader-peek-char
o 65537
o 65536
y void?
y not
y if
o 17179869186
y void
y return
o 266287972354
y @auto-symbol-14
o 65536
y whitespace?
y not
y if
o 124554051586
y @auto-symbol-7
b true
y not
y if
o 94489280514
y reader-peek-char
o 65537
o 65536
y void?
o 65536
y whitespace?
y not
y or
y not
y if
o 17179869186
o 0
y return
o 25769803778
y @auto-symbol-6
o 0
y reader-next-char
y +
o 1
y @auto-symbol-5
o 107374182403
y @auto-symbol-8
o 120259084290
y @auto-symbol-15
y @auto-symbol-11
b true
y not
y if
o 90194313218
y reader-peek-char
o 65537
o 65536
y void?
o 65536
y whitespace?
y or
y not
y if
o 17179869186
o 0
y return
o 25769803778
y @auto-symbol-10
o 0
y reader-next-char
y +
o 1
y @auto-symbol-9
o 103079215107
y @auto-symbol-12
y @auto-symbol-13
W reader-skip-whitespace
L 30
y @auto-symbol-19
b true
y not
y if
o 107374182402
y reader-peek-char
o 1
o 0
y void?
y not
y if
o 12884901890
y return
o 60129542146
y @auto-symbol-17
o 0
y ord
i 32
y >
y not
y if
o 12884901890
y return
o 17179869186
y @auto-symbol-18
y reader-next-char
y drop
y @auto-symbol-16
o 120259084291
y @auto-symbol-20
W reader-next
L 51
y reader-skip-whitespace
s 
o 1
y @auto-symbol-24
b true
y not
y if
o 115964116994
y reader-peek-char
o 65537
o 65536
y void?
y not
y if
o 12884901890
o 85899345922
o 68719476738
y @auto-symbol-22
o 65536
y ord
i 32
y <=
y not
y if
o 12884901890
o 42949672962
o 25769803778
y @auto-symbol-23
o 0
y reader-next-char
y +
o 1
y @auto-symbol-21
o 128849018883
y @auto-symbol-25
y @return-chunk
o 0
y length
i 0
y ==
y not
y if
o 17179869186
y void
y return
o 21474836482
y @auto-symbol-27
o 0
y string->symbol
y return
y @auto-symbol-26
W reader-peek-next
L 7
y READER-POS
o 65537
y reader-next
o 131073
o 65536
y READER-POS!
o 131072
W digit?
L 37
o 1
o 0
y string?
y not
y if
o 42949672962
o 0
s 0
y >=
o 0
s 9
y <=
y and
y return
o 94489280514
y @auto-symbol-29
o 0
y symbol?
y not
y if
o 42949672962
o 0
y '0
y >=
o 0
y '9
y <=
y and
y return
o 30064771074
y @auto-symbol-30
s digit?%32requires%32string%32or%32symbol%32but%32got:
o 0
y str
y +
y error
y @auto-symbol-28
W any-digits?
L 4
o 1
o 0
F
L 1
y digit?
y any?
W ltrim-char
L 18
o 65537
o 1
o 0
i 0
y get
o 65536
y ==
y not
y if
o 25769803778
o 0
i 1
i -1
y slice
o 12884901890
y @auto-symbol-32
o 0
y @auto-symbol-31
W ltrim-sign
L 23
o 1
o 0
i 0
y get
s +
y ==
o 0
i 0
y get
s -
y ==
y or
y not
y if
o 25769803778
o 0
i 1
i -1
y slice
o 12884901890
y @auto-symbol-34
o 0
y @auto-symbol-33
W ltrim-digits
L 31
o 1
y @auto-symbol-37
o 0
y length
i 0
y >
y not
y if
o 85899345922
o 0
i 0
y get
y digit?
y not
y not
y if
o 17179869186
o 0
y return
o 30064771074
y @auto-symbol-36
o 0
i 1
i -1
y slice
o 1
y @auto-symbol-35
o 111669149699
y @auto-symbol-38
o 0
y return
W int-string?
L 19
o 1
o 0
y any-digits?
y not
y not
y if
o 17179869186
b false
y return
o 38654705666
y @auto-symbol-40
o 0
y ltrim-sign
y ltrim-digits
y length
i 0
y ==
y return
y @auto-symbol-39
W int-symbol?
L 4
o 1
o 0
y str
y int-string?
W float-string?
L 28
o 1
o 0
y any-digits?
y not
y not
y if
o 17179869186
b false
y return
o 77309411330
y @auto-symbol-42
o 0
y ltrim-sign
y ltrim-digits
s .
y ltrim-char
y ltrim-digits
s e
y ltrim-char
s E
y ltrim-char
y ltrim-sign
y ltrim-digits
y length
i 0
y ==
y return
y @auto-symbol-41
W float-symbol?
L 2
y str
y float-string?
W hex-char-to-int
L 57
o 1
o 0
s 0
y >=
o 0
s 9
y <=
y and
y if
o 98784247810
o 0
s a
y >=
o 0
s h
y <=
y and
y if
o 90194313218
o 0
s A
y >=
o 0
s H
y <=
y and
y if
o 90194313218
s Bad%32hex%32digit:%32
o 0
y +
y error
y @digit
o 0
y ord
s 0
y ord
y -
y return
y @hexlower
o 0
y ord
s a
y ord
y -
i 10
y +
y return
y @hexupper
o 0
y ord
s A
y ord
y -
i 10
y +
y return
W parse-hex
L 5
o 1
o 0
F
L 8
o 65537
o 1
o 65536
y hex-char-to-int
o 0
i 16
y *
y +
i 0
y fold
W int4-to-hex
L 28
y dup
i 9
y <=
y if
o 47244640258
y dup
i 16
y <
y if
o 51539607554
s Bad%32value%32in%32int4-to-hex:
y swap
y str
y +
y error
y @digit
s 0
y ord
y +
y chr
y return
y @hex
i 10
y -
s a
y ord
y +
y chr
W int8-to-hex
L 6
i 16
y /mod
y int4-to-hex
y swap
y int4-to-hex
y +
W int16-to-hex
L 6
i 256
y /mod
y int8-to-hex
y swap
y int8-to-hex
y +
W int32-to-hex
L 6
i 65536
y /mod
y int16-to-hex
y swap
y int16-to-hex
y +
W str.hex
L 1
y int32-to-hex
W parse-binary
L 5
o 1
o 0
F
L 40
o 65537
o 1
o 65536
s 0
y ==
y not
y if
o 21474836482
o 0
i 1
y bit-shl
o 120259084290
y @auto-symbol-44
o 65536
s 1
y ==
y not
y if
o 30064771074
o 0
i 1
y bit-shl
i 1
y bit-or
o 64424509442
y @auto-symbol-45
o 65536
s .
y ==
y not
y if
o 12884901890
o 0
o 25769803778
y @auto-symbol-46
s Bad%32binary%32digit:%32
o 65536
y +
y error
y @auto-symbol-43
i 0
y fold
W syntax-next
L 249
y reader-next
o 65537
o 65536
y void?
y not
y if
o 17179869186
y void
y return
o 1026497183746
y @auto-symbol-52
o 65536
y '(
y ==
y not
y if
o 21474836482
y syntax-comment
y syntax-next
y return
o 979252543490
y @auto-symbol-53
o 65536
y '\
y ==
y not
y if
o 21474836482
y syntax-line-comment
y syntax-next
y return
o 932007903234
y @auto-symbol-54
o 65536
y '{
y ==
y not
y if
o 17179869186
y syntax-v2-closure
y return
o 889058230274
y @auto-symbol-55
o 65536
y '[
y ==
y not
y if
o 17179869186
y syntax-list
y return
o 846108557314
y @auto-symbol-56
o 65536
y '#op(
y ==
y not
y if
o 17179869186
y syntax-opcode
y return
o 803158884354
y @auto-symbol-57
o 65536
y ':
y ==
y not
y if
o 17179869186
y v2-syntax-define-word
y return
o 760209211394
y @auto-symbol-58
o 65536
y 'del
y ==
y not
y if
o 17179869186
y syntax-del
y return
o 717259538434
y @auto-symbol-59
o 65536
y 'true
y ==
y not
y if
o 17179869186
b true
y return
o 674309865474
y @auto-symbol-60
o 65536
y 'false
y ==
y not
y if
o 17179869186
b false
y return
o 631360192514
y @auto-symbol-61
o 65536
y 'null
y ==
y not
y if
o 17179869186
n 
y return
o 588410519554
y @auto-symbol-62
o 65536
y 'WHILE
y ==
y not
y if
o 17179869186
y syntax-while
y return
o 545460846594
y @auto-symbol-63
o 65536
y 'IF
y ==
y not
y if
o 17179869186
y syntax-if
y return
o 502511173634
y @auto-symbol-64
o 65536
i 0
i 3
y slice
y TRIPLEQUOTE
y ==
y not
y if
o 21474836482
o 65536
y syntax-triple-string
y return
o 442381631490
y @auto-symbol-65
o 65536
i 0
y get
y '"
y ==
y not
y if
o 21474836482
o 65536
y syntax-string
y return
o 386547056642
y @auto-symbol-66
o 65536
y int-symbol?
y not
y if
o 21474836482
o 65536
y parse-int
y return
o 343597383682
y @auto-symbol-67
o 65536
y float-symbol?
y not
y if
o 21474836482
o 65536
y parse-float
y return
o 300647710722
y @auto-symbol-68
o 65536
i 0
y get
y '#
y ==
y not
y if
o 249108103170
o 65536
i 1
i -1
y slice
y float-symbol?
y not
y if
o 34359738370
o 65536
i 1
i -1
y slice
y parse-float
y return
o 176093659138
y @auto-symbol-48
o 65536
i 1
y get
y 'x
y ==
y not
y if
o 38654705666
o 65536
i 2
i -1
y slice
y str
y parse-hex
y return
o 103079215106
y @auto-symbol-49
o 65536
i 1
y get
y 'b
y ==
y not
y if
o 38654705666
o 65536
i 2
i -1
y slice
y str
y parse-binary
y return
o 30064771074
y @auto-symbol-50
s Unknown%32literal:%32
o 65536
y str
y +
y error
y @auto-symbol-47
o 17179869186
y @auto-symbol-69
o 65536
y return
y @auto-symbol-51
W syntax-collect-until-any-symbol
L 53
o 1
L 0
o 65537
y @auto-symbol-75
b true
y not
y if
o 193273528322
y syntax-next
o 196609
o 196608
y void?
y not
y if
o 30064771074
s End%32of%32input%32looking%32for:%32
o 0
y str
y +
y error
o 128849018882
y @auto-symbol-73
o 0
o 196608
y contains?
y not
y if
o 21474836482
o 65536
o 196608
y return
o 81604378626
y @auto-symbol-74
o 196608
y is-inline-list?
y not
y if
o 34359738370
o 65536
o 196608
i 1
i -1
y slice
y extend!
o 21474836482
y @auto-symbol-71
o 65536
o 196608
y append!
y @auto-symbol-70
y @auto-symbol-72
o 206158430211
y @auto-symbol-76
W syntax-collect-until-symbol
L 6
o 1
o 0
i 1
y make-list
y syntax-collect-until-any-symbol
y drop
W make-unique-symbol
L 9
y NEXT-UNIQUE-SYMBOL-NUMBER
i 1
y +
y NEXT-UNIQUE-SYMBOL-NUMBER!
y 'auto-symbol-
y NEXT-UNIQUE-SYMBOL-NUMBER
y str
y string->symbol
y +
W syntax-while
L 46
y 'DO
y syntax-collect-until-symbol
o 131073
y 'END
y syntax-collect-until-symbol
o 196609
y ',,inline
i 1
y make-list
o 1
y make-unique-symbol
o 262145
y make-unique-symbol
o 327681
o 0
y '@
o 262144
y +
y append!
o 0
o 131072
y extend!
o 0
y 'not
y 'if
y '>>
o 327680
y +
i 3
y make-list
y extend!
o 0
o 196608
y extend!
o 0
y '<<
o 262144
y +
y '@
o 327680
y +
i 2
y make-list
y extend!
o 0
y return
W syntax-if
L 74
L 0
o 196609
y @loop
y 'THEN
y syntax-collect-until-symbol
o 65537
y @read-do
L 3
y ELIF
y ELSE
y END
y syntax-collect-until-any-symbol
o 262145
o 131073
o 196608
o 65536
o 131072
i 2
y make-list
y append!
y 'ELIF
o 262144
y ==
y if
o 68719476738
y 'ELSE
o 262144
y ==
y if
o 73014444034
y 'END
o 262144
y ==
y if
o 111669149698
s Expecting%32[ELIF%32|%32ELSE%32|%32END],%32got:%32
o 262144
y str
y +
y error
y @got-elif
o 65536
y null?
y if
o 42949672962
o 171798691843
y @got-else
o 65536
y null?
y if
o 30064771074
n 
o 65537
o 188978561027
y @bad-elif
s Syntax%32error:%32Got%32ELIF%32after%32ELSE
y error
y @bad-else
s Syntax%32error:%32Got%32ELSE%32after%32ELSE
y error
y @got-end
y ',,inline
i 1
y make-list
o 1
y make-unique-symbol
o 327681
o 196608
F
L 32
o 65537
o 1
o 0
y null?
y if
o 98784247810
y make-unique-symbol
o 393473
o 256
o 0
y extend
L 2
y not
y if
y extend
y '>>
o 393472
y +
y append
o 65536
y extend
y '>>
o 327936
y +
y append
y '@
o 393472
y +
y append!
y return
y @make-else
o 256
o 65536
y extend!
y bind-lambda
y apply-for-each
o 0
y '@
o 327680
y +
y append!
o 0
W syntax-parse-decl-arglist
L 36
y ',,declargs
i 1
y make-list
o 1
y @auto-symbol-80
b true
y not
y if
o 115964116994
y syntax-next
o 65537
o 65536
y void?
y not
y if
o 17179869186
s End%32of%32input%32inside%32@(
y error
o 64424509442
y @auto-symbol-78
o 65536
y ')
y ==
y not
y if
o 17179869186
o 0
y return
o 21474836482
y @auto-symbol-79
o 0
o 65536
y append!
y @auto-symbol-77
o 128849018883
y @auto-symbol-81
W is-inline-list?
L 26
o 1
o 0
y list?
y not
y if
o 77309411330
o 0
y length
i 0
y >
y not
y if
o 34359738370
o 0
i 0
y get
y ',,inline
y ==
y return
o 8589934594
y @auto-symbol-83
y @auto-symbol-82
o 8589934594
y @auto-symbol-85
y @auto-symbol-84
b false
W v2-syntax-define-word
L 69
y syntax-next
y dup
y symbol?
y not
y if
o 180388626434
o 65537
L 0
o 131073
y reader-peek-next
y '(
y ==
y if
o 8589934594
o 38654705666
y @read-arglist
y reader-next
y drop
y syntax-parse-decl-arglist
o 131072
y swap
y append
o 131073
y @loop
y syntax-next
y dup
y void?
y if
o 64424509442
y dup
y ';
y ==
y if
o 124554051586
y dup
y is-inline-list?
y if
o 68719476738
o 131072
y swap
y append
o 131073
o 81604378627
y @eof
y drop
s End%32of%32input%32inside%32:%32...%32;
y error
y @badname
s Expecting%32symbol%32after%32':'%32but%32got:%32
y swap
y repr
y +
y error
y @inline-list
i 1
i -1
y slice
o 131072
y swap
y extend
y drop
o 163208757251
y @done
y drop
y @finish
o 131072
o 65536
y save-defined-word
y syntax-next
W syntax-del
L 21
y syntax-next
y dup
y symbol?
y not
y if
o 42949672962
y ''
y swap
y +
y ',,inline
y swap
y ',,del
i 3
y make-list
y return
y @badname
y str
s Bad%32name%32in%32del:%32
y swap
y +
y error
W syntax-opcode
L 70
L 0
o 1
y @loop
y reader-next
o 65537
o 65536
y void?
y if
o 47244640258
o 65536
y ')
y ==
y if
o 38654705666
o 0
o 65536
y append
o 1
o 68719476739
y @eof
s Unexpected%32end%32of%32input%32inside%32#op(%32..%32)
y error
y @closelist
o 0
y length
i 0
y ==
y if
o 141733920770
o 0
y length
i 4
y >
y if
o 128849018882
y @padlist
o 0
y length
i 4
y ==
y if
o 25769803778
o 0
s 0
y append
o 1
o 47244640259
y @make-opcode
o 0
i 0
y get
o 0
i 1
i -1
y slice
F
L 1
y parse-int
y map
y unmake
y drop
y make-opcode
y return
y @no-name
s Missing%32opcode%32name
y error
y @too-long
s #op(%32..%32)%32too%32long:%32
o 0
y str
y +
y error
W syntax-string
L 58
y symbol->string
y dup
i -1
y get
y ord
i 34
y ==
y over
y length
i 1
y >
y and
y if
o 158913789954
y @loop
y reader-next-char
y dup
y void?
y if
o 107374182402
y dup
y ord
i 34
y ==
y if
o 12884901890
y +
o 55834574851
y @quote
y +
y reader-peek-char
y dup
y void?
y if
o 30064771074
y dup
y whitespace?
y if
o 12884901890
y drop
o 111669149699
y @endquote
y drop
o 30064771074
y @eof
y drop
s Unexpected%32end%32of%32input%32inside%32string:%32
y swap
y +
y error
y @endstring
y dup
y length
i 2
y -
i 1
y swap
y slice
W syntax-triple-string
L 72
o 1
o 0
y symbol->string
o 1
o 0
y length
i 6
y >=
o 0
i -3
i -1
y slice
y TRIPLEQUOTE
y symbol->string
y ==
y and
y not
y if
o 12884901890
o 188978561026
o 8589934594
y @auto-symbol-87
y @auto-symbol-86
y @auto-symbol-92
b true
y not
y if
o 150323855362
y reader-next-chunk
o 131073
o 131072
y void?
y not
y if
o 30064771074
s String%32ended%32inside%32triple-quoted%32string:%32
o 0
y str
y +
y error
o 25769803778
y @auto-symbol-89
o 0
o 131072
y +
o 1
y @auto-symbol-88
o 0
i -3
i -1
y slice
y TRIPLEQUOTE
y symbol->string
y ==
y not
y if
o 12884901890
o 25769803778
o 8589934594
y @auto-symbol-91
y @auto-symbol-90
o 163208757251
y @auto-symbol-93
y @return
o 0
o 0
y length
i 3
y swap
i 6
y -
y slice
W syntax-list
L 48
i 0
y make-list
o 1
y @auto-symbol-98
b true
y not
y if
o 171798691842
y syntax-next
o 65537
o 65536
y void?
y not
y if
o 17179869186
s Unexpected%32end%32of%32input%32inside%32[%32..%32]
y error
o 120259084290
y @auto-symbol-95
o 65536
y ']
y ==
y not
y if
o 17179869186
o 0
y return
o 77309411330
y @auto-symbol-96
o 65536
y is-inline-list?
y not
y if
o 34359738370
o 0
o 65536
i 1
i -1
y slice
y extend!
o 21474836482
y @auto-symbol-97
o 0
o 65536
y append!
y @auto-symbol-94
o 184683593731
y @auto-symbol-99
W syntax-comment
L 61
s 
o 65537
i 1
o 1
y @loop
y reader-next
y dup
y void?
y if
o 184683593730
y dup
y symbol->string
o 65536
s %32
y +
y swap
y +
o 65537
y dup
y ')
y ==
y if
o 34359738370
y dup
y '(
y ==
y if
o 64424509442
y drop
o 107374182403
y @closeparen
o 0
i 1
y -
o 1
o 0
i 0
y ==
y if
o 42949672962
y drop
o 158913789955
y @openparen
o 0
i 1
y +
o 1
y drop
o 188978561027
y @end
y drop
y return
y @eof
o 65536
i 0
i 40
y slice
s Unexpected%32end%32of%32input%32inside%32comment:%32
y swap
y +
y error
W syntax-line-comment
L 22
y @loop
y reader-next-char
y dup
y void?
y if
o 64424509442
y dup
y ord
i 13
y ==
y if
o 38654705666
y dup
y ord
i 10
y ==
y if
o 12884901890
y drop
o 81604378627
y @eol
y drop
W syntax-v2-closure
L 53
L 0
o 1
y reader-peek-next
y '(
y ==
y if
o 8589934594
o 38654705666
y @read-arglist
y reader-next
y drop
y syntax-parse-decl-arglist
o 0
y swap
y append
o 1
y @loop
y syntax-next
y dup
y void?
y if
o 64424509442
y dup
y '}
y ==
y if
o 98784247810
y dup
y is-inline-list?
y if
o 42949672962
o 0
y swap
y append
o 1
o 81604378627
y @eof
y drop
s End%32of%32input%32inside%32{%32..%32}
y error
y @inline-list
i 1
i -1
y slice
o 0
y swap
y extend
y drop
o 137438953475
y @done
y drop
o 0
y make-lambda
W serialize-escape-string
L 2
F
L 65
y dup
y ord
i 32
y ==
y if
o 111669149698
y dup
y ord
i 37
y ==
y if
o 146028888066
y dup
y ord
i 9
y ==
y if
o 90194313218
y dup
y ord
i 10
y ==
y if
o 124554051586
y dup
y ord
i 13
y ==
y if
o 128849018882
y return
y @space
y drop
i 37
y chr
s 32
y +
y return
y @tab
y drop
i 37
y chr
s 09
y +
y return
y @percent
y drop
i 37
y chr
s 37
y +
y return
y @lf
y drop
i 37
y chr
s 10
y +
y return
y @cr
y drop
i 37
y chr
s 13
y +
y map
W serialize-object
L 108
y dup
y int?
y if
o 163208757250
y dup
y float?
y if
o 176093659138
y dup
y string?
y if
o 244813135874
y dup
y symbol?
y if
o 257698037762
y dup
y list?
y if
o 270582939650
y dup
y lambda?
y if
o 300647710722
y dup
y bool?
y if
o 120259084290
y dup
y null?
y if
o 133143986178
y dup
y opcode?
y if
o 279172874242
y repr
s Unknown%32obj%32in%32serialize-object:
y swap
y +
y error
y @int
s i
y .
y str
y puts
y CR
y return
y @float
s f
y .
y str
y puts
y CR
y return
y @bool
s b
y .
y str
y puts
y CR
y return
y @null
y drop
s n
y .
y CR
y return
y @string
s s
y .
y serialize-escape-string
y puts
y CR
y return
y @symbol
s y
y .
y str
y puts
y CR
y return
y @list
s L
y .
y dup
y length
y str
y puts
y CR
F
L 1
y serialize-object
y for-each
y return
y @lambda
s F
y puts
y CR
y unmake
y serialize-object
y return
y @opcode
s o
y .
y opcode-packed
y str
y puts
y CR
y return
W save-defined-word
L 7
y swap
i 2
y make-list
y COMPILED-NAMELISTS
y swap
y append
y drop
W compile-split-declargs
L 53
o 1
L 0
o 131073
L 0
o 196609
b false
o 262145
i 1
o 65537
y @loop
o 65536
o 0
y length
y >=
y if
o 150323855362
o 0
o 65536
y get
y '--
y ==
y if
o 81604378626
o 262144
y if
o 34359738370
o 131072
o 0
o 65536
y get
y append
o 131073
o 51539607554
y @add-to-locals
o 196608
o 0
o 65536
y get
y append
o 196609
o 17179869186
y @got-dashes
b true
o 262145
y @next
o 65536
i 1
y +
o 65537
o 171798691843
y @done
o 131072
o 196608
W compile-collect-args-locals
L 131
o 1
L 0
o 65537
L 0
o 131073
L 0
o 196609
i 0
o 262145
y @loop
o 0
o 262144
y get
y dup
y void?
y if
o 472446402562
y dup
y list?
y if
o 51539607554
y dup
y '@args
y ==
y if
o 158913789954
y dup
y '@locals
y ==
y if
o 261993005058
o 107374182402
y @check-list
y dup
y length
i 0
y ==
y if
o 77309411330
y dup
i 0
y get
y ',,declargs
y !=
y if
o 47244640258
y compile-split-declargs
o 196608
y swap
y +
o 196609
o 131072
y swap
y +
o 131073
o 279172874242
y @keep-obj
o 65536
y swap
y append
o 65537
o 253403070466
y @args-list
y drop
o 262144
i 1
y +
o 262145
o 0
o 262144
y get
y dup
y void?
y if
o 47244640258
y dup
y list?
y not
y if
o 25769803778
o 131072
y swap
y +
o 131073
o 154618822658
y @missing-args
s Expecting%32list%32after%32@args%32in:
o 0
y str
y +
y error
y @locals-list
y drop
o 262144
i 1
y +
o 262145
o 0
o 262144
y get
y dup
y void?
y if
o 47244640258
y dup
y list?
y not
y if
o 25769803778
o 196608
y swap
y +
o 196609
o 30064771074
y @missing-locals
s Expecting%32list%32after%32@locals%32in:
o 0
y str
y +
y error
y @next
o 262144
i 1
y +
o 262145
o 498216206339
y @done
y drop
o 65536
o 131072
o 196608
W new-closure-env
L 6
y ',,closure-env
L 0
y make-dict
y void
i 3
y make-list
W closure-env?
L 6
o 1
o 0
i 0
y get
y ',,closure-env
y ==
W closure-env-dict
L 4
o 1
o 0
i 1
y get
W closure-env-set-up
L 7
o 65537
o 1
o 0
i 2
o 65536
y put
y drop
W closure-env-get-up
L 4
o 1
o 0
i 2
y get
W closure-env-add-name
L 38
o 65537
o 1
o 0
y closure-env?
y not
y if
o 111669149698
o 0
y closure-env-dict
o 65536
y str
y get
y void?
y not
y if
o 47244640258
o 0
y closure-env-dict
o 65536
y str
o 0
y closure-env-dict
y length
y put
y drop
y return
y @dup-name
s Duplicate%32name%32in%32args/locals:%32
o 65536
y str
y +
y error
y @bad-env
s Not%32a%32closure%32environment:%32
o 0
y str
y +
y error
W closure-env-add-namelist
L 6
o 65537
o 1
o 65536
F
L 4
o 1
o 256
o 0
y closure-env-add-name
y bind-lambda
y for-each
W closure-env-lookup-name
L 6
o 65537
o 1
o 0
o 65536
i 0
y closure-env-lookup-name-inner
W closure-env-lookup-name-inner
L 50
o 131073
o 65537
o 1
o 65536
i -1
y get
y '!
y !=
y if
o 38654705666
o 65536
i 0
o 65536
y length
i 1
y -
y slice
o 65537
y @lookup
o 0
y closure-env-dict
o 65536
y str
y get
o 196609
o 196608
y void?
y if
o 17179869186
o 131072
o 196608
y return
y @try-up
o 0
y closure-env-get-up
o 262145
o 262144
y void?
y if
o 34359738370
o 262144
o 65536
o 131072
i 1
y +
y closure-env-lookup-name-inner
y return
y @not-found
y void
y void
W closure-env-make-ref-or-set
L 23
o 131073
o 65537
o 1
o 0
i -1
y get
y '!
y ==
y if
o 30064771074
y 'FRAME-GET
o 65536
o 131072
i 0
y make-opcode
y return
y @set
y 'FRAME-SET
o 65536
o 131072
i 0
y make-opcode
y return
W v2-compile-rewrite-args-locals
L 222
o 131073
o 65537
o 1
o 0
y compile-collect-args-locals
o 393217
o 327681
o 196609
i -1
o 262145
o 196608
o 1
L 0
o 196609
F
L 9
o 1
o 196864
y 'FRAME-SET
i 0
o 0
i 0
y make-opcode
y append
y drop
y bind-lambda
o 327680
y length
i 1
y -
i -1
o 327680
y length
y for-count
o 131072
y if
o 64424509442
y new-closure-env
o 458753
o 327680
F
L 4
o 1
o 459008
o 0
y closure-env-add-name
y bind-lambda
y for-each
o 393216
F
L 14
o 1
o 459008
y closure-env-dict
o 0
y str
y get
y void?
y not
y if
o 17179869186
o 459008
o 0
y closure-env-add-name
y @dup-name
y bind-lambda
y for-each
o 458752
o 65536
y closure-env-set-up
o 17179869186
y @make-env-plain-list
o 65536
o 458753
y @begin-rewrite
i 0
o 524289
y @rewrite-loop
o 0
o 524288
y get
y void?
y if
o 708669603842
o 0
o 524288
y get
y symbol?
y if
o 180388626434
o 0
o 524288
y get
y list?
y if
o 459561500674
o 0
o 524288
y get
y lambda?
y if
o 240518168578
y @copy-element
o 131072
y not
y if
o 73014444034
o 0
o 524288
y get
y symbol?
y not
y if
o 42949672962
o 196608
y ''
o 0
o 524288
y get
y +
y append
o 196609
o 515396075522
y @normal-copy-element
o 196608
o 0
o 524288
y get
y append
o 196609
o 481036337154
y @symbol
o 0
o 524288
y get
o 720897
o 458752
o 720896
y closure-env-lookup-name
o 655361
o 589825
o 655360
y void?
y if
o 180388626435
o 196608
o 720896
o 589824
o 655360
y closure-env-make-ref-or-set
y append
o 196609
o 262144
o 589824
y max
o 262145
o 369367187458
y @rewrite-inner-lambda
o 0
o 524288
y get
y unmake
o 458752
b false
y v2-compile-rewrite-args-locals
o 851969
o 786433
o 196608
o 786432
y make-lambda
y append
o 196609
o 851968
i 0
y <=
y if
o 21474836482
o 196608
y 'bind-lambda
y append
o 196609
y @done-rewrite-sub
o 851968
i 0
y >
y not
y if
o 34359738370
o 262144
o 851968
i 1
y -
y max
o 262145
o 25769803778
y @auto-symbol-101
o 262144
o 851968
y max
o 262145
y @auto-symbol-100
o 176093659138
y @rewrite-inner-plain-list
o 0
o 524288
y get
y length
o 917505
o 0
o 524288
y get
o 458752
b true
y v2-compile-rewrite-args-locals
o 851969
o 786433
o 851968
i 0
y <
y if
o 60129542146
o 196608
o 786432
y extend
o 917504
y append
y 'make-list
y append
o 196609
o 262144
o 851968
y max
o 262145
o 38654705666
y @plain-list-no-rewrites
o 196608
o 0
o 524288
y get
y append
o 196609
o 4294967298
y @next
o 524288
i 1
y +
o 524289
o 730144440323
y @done
o 196608
o 262144
y return
W compiler-find-label
L 35
o 196609
o 131073
o 65537
o 1
o 131072
o 327681
y @loop
o 0
o 327680
y get
y void?
y if
o 85899345922
o 0
o 327680
y get
y '@
o 65536
y +
y ==
y if
o 25769803778
o 327680
o 196608
y +
o 327681
o 85899345923
y @found
o 327680
o 131072
y -
y return
y @notfound
n 
y return
W compiler-optimize-jumps
L 126
o 1
i 0
o 65537
L 0
o 131073
y @loop
o 0
o 65536
y get
o 196609
o 196608
y void?
y if
o 476741369858
o 196608
y symbol?
y if
o 85899345922
o 196608
y list?
y if
o 347892350978
o 196608
y lambda?
y if
o 360777252866
y @copy-element
o 131072
o 196608
y append
y drop
y @next
o 65536
i 1
y +
o 65537
o 133143986179
y @symbol
o 196608
i 0
i 2
y slice
y '<<
y ==
y if
o 42949672962
o 196608
i 0
i 2
y slice
y '>>
y ==
y if
o 111669149698
o 120259084291
y @backward-jump
o 0
o 196608
i 2
i -1
y slice
o 65536
i -1
y compiler-find-label
o 262145
o 262144
y null?
y if
o 214748364802
o 131072
y 'JUMP-BACK
i 0
i 0
o 262144
y neg
y make-opcode
y append
y drop
o 201863462915
y @forward-jump
o 0
o 196608
i 2
i -1
y slice
o 65536
i 1
y compiler-find-label
o 262145
o 262144
y null?
y if
o 111669149698
o 131072
y 'JUMP-FORW
i 0
i 0
o 262144
y make-opcode
y append
y drop
o 300647710723
y @list
o 131072
o 196608
y compiler-optimize-jumps
y append
y drop
o 330712481795
y @lambda
o 131072
o 196608
y unmake
y compiler-optimize-jumps
y make-lambda
y append
y drop
o 369367187459
y @missing-label
s Cannot%32find%32label%32for%32jump:%32
o 196608
y str
y +
y error
y @done
o 131072
W compile-finalize-word
L 8
o 1
o 0
y void
b false
y v2-compile-rewrite-args-locals
y drop
y compiler-optimize-jumps
y return
W compiler-finalize-all-words
L 27
i 0
y @loop
y dup
y COMPILED-NAMELISTS
y length
y >=
y if
o 77309411330
y COMPILED-NAMELISTS
y over
y get
i 1
y get
y compile-finalize-word
y over
y COMPILED-NAMELISTS
y swap
y get
i 1
y rot
y put
y drop
i 1
y +
o 98784247811
y @done
y drop
W byte-compile
L 36
i 0
y make-list
y COMPILED-NAMELISTS!
L 0
o 131073
y @loop
y syntax-next
y dup
y void?
y if
o 81604378626
y dup
y is-inline-list?
y if
o 25769803778
o 131072
y swap
y append
o 131073
o 60129542147
y @inline-list
i 1
i -1
y slice
o 131072
y swap
y extend
y drop
o 98784247811
y @done
y drop
o 131072
y '__main__
y save-defined-word
y compiler-finalize-all-words
y COMPILED-NAMELISTS
W byte-compile-file
L 4
o 1
o 0
y reader-open-file
y byte-compile
W byte-compile-string
L 4
o 1
o 0
y reader-open-string
y byte-compile
W serialize-compiled-output
L 2
F
L 11
s W
y .
y dup
i 0
y get
y str
y puts
y CR
i 1
y get
y serialize-object
y for-each
W compile-and-serialize
L 19
o 1
o 0
y byte-compile-file
s M%32v1:
y puts
o 0
y file-read
y fnv-1a-32
y str
y puts
y CR
s L
y .
y dup
y length
y str
y puts
y CR
y serialize-compiled-output
W compile-and-load-string
L 5
o 1
o 0
y byte-compile-string
F
L 7
y dup
i 1
y get
y swap
i 0
y get
y make-word
y for-each
W check-needs-recompile
L 38
o 1
o 0
s .b
y +
y file-exists?
y not
y if
o 124554051586
o 0
s .b
y +
y stream-reader-open-file
y stream-reader-next-line
o 65537
y drop
o 65536
i 0
i 5
y slice
s M%32v1:
y !=
y if
o 60129542146
o 65536
i 5
i -1
y slice
y parse-int
o 0
y file-read
y fnv-1a-32
y !=
y if
o 12884901890
b false
y return
y @need-compile
b true
W cached-compile-and-load
L 24
o 1
o 0
y check-needs-recompile
y if
o 25769803778
o 0
s .b
y +
y deserialize
y return
y @do-compile
o 0
s .b
y +
y open-as-stdout
o 0
y compile-and-serialize
y void
y open-as-stdout
o 0
s .b
y +
y deserialize
y return
W __main__
L 22
y 'READER-TEXT
y void
y make-var
y 'READER-TEXT-LEN
y void
y make-var
y 'READER-POS
y void
y make-var
y 'TRIPLEQUOTE
i 34
i 34
i 34
i 3
y make-symbol
y make-var
y 'NEXT-UNIQUE-SYMBOL-NUMBER
i 0
y make-var
y 'COMPILED-NAMELISTS
y void
y make-var
