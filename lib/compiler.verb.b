L 38
W reader-open-string
L 10
y READER_TEXT
y set!
y READER_TEXT
y ref
y length
y READER_TEXT_LEN
y set!
i 0
y READER_POS
y set!
W reader-open-file
L 2
y read-file
y reader-open-string
W reader-peek-char
L 15
y READER_POS
y ref
y READER_TEXT_LEN
y ref
y >=
y if
y >>eof
y READER_TEXT
y ref
y READER_POS
y ref
y get
y return
y @eof
y void
W reader-next-char
L 21
y READER_POS
y ref
y READER_TEXT_LEN
y ref
y >=
y if
y >>eof
y READER_TEXT
y ref
y READER_POS
y ref
y get
y READER_POS
y ref
i 1
y +
y READER_POS
y set!
y return
y @eof
y void
W reader-next-chunk
L 45
s 
y reader-peek-char
y dup
y void?
y if
y >>drop-return
y dup
y whitespace?
y if
y >>ws
y drop
y @non-ws-loop
y reader-peek-char
y dup
y void?
y if
y >>drop-return
y dup
y whitespace?
y if
y >>drop-return
y drop
y reader-next-char
y +
y <<non-ws-loop
y @ws
y drop
y @ws-loop
y reader-peek-char
y dup
y void?
y if
y >>drop-return
y dup
y whitespace?
y not
y if
y >>drop-return
y drop
y reader-next-char
y +
y <<ws-loop
y @drop-return
y drop
y return
W reader-next
L 49
s 
y @skipws
y reader-peek-char
y dup
y void?
y if
y >>void-on-read
y ord
i 32
y >
y if
y >>word-loop
y reader-next-char
y drop
y <<skipws
y @void-on-read
y drop
y >>eof
y @word-loop
y reader-peek-char
y dup
y void?
y if
y <<void-on-read
y dup
y ord
i 32
y <=
y if
y >>space
y drop
y reader-next-char
y +
y <<word-loop
y @space
y drop
y @eof
y dup
y length
i 0
y ==
y if
y >>isvoid
y string->symbol
y return
y @isvoid
y drop
y void
y return
W whitespace?
L 5
i 0
y get
y ord
i 32
y <=
W digit?
L 34
y dup
y string?
y if
y >>string
y dup
y '0
y <
y if
y >>notdigit
y dup
y '9
y >
y if
y >>notdigit
y drop
b true
y return
y @string
y dup
s 0
y <
y if
y >>notdigit
y dup
s 9
y >
y if
y >>notdigit
y drop
b true
y return
y @notdigit
y drop
b false
W any-digits?
L 2
F
L 1
y digit?
y any
W ltrim-char
L 21
y over
y length
i 0
y !=
y if
y >>do-trim
y drop
y return
y @do-trim
y >L
i 0
y +get
y L>
y ==
y if
y >>trim
y return
y @trim
i 1
i -1
y slice
W ltrim-sign
L 25
y dup
y length
i 0
y !=
y if
y >>do-trim
y return
y @do-trim
i 0
y +get
s +
y ==
y if
y >>trim
i 0
y +get
s -
y ==
y if
y >>trim
y return
y @trim
i 1
i -1
y slice
W ltrim-digits
L 18
y @loop
y dup
y length
i 0
y ==
y if
y >>done
i 0
y +get
y digit?
y not
y if
y >>done
i 1
i -1
y slice
y <<loop
y @done
W int-string?
L 14
y dup
y any-digits?
y not
y if
y >>nodigits
y ltrim-sign
y ltrim-digits
y length
i 0
y ==
y return
y @nodigits
y drop
b false
W int-symbol?
L 3
y unmake
y make-string
y int-string?
W float-string?
L 21
y dup
y any-digits?
y not
y if
y >>nodigits
y ltrim-sign
y ltrim-digits
s .
y ltrim-char
y ltrim-digits
s e
y ltrim-char
y ltrim-sign
y ltrim-digits
y length
i 0
y ==
y return
y @nodigits
y drop
b false
W float-symbol?
L 3
y unmake
y make-string
y float-string?
W syntax-next
L 162
y reader-next
y dup
y void?
y if
y >>void
y dup
y '(
y ==
y if
y >>comment
y dup
y '\
y ==
y if
y >>line-comment
y dup
y '{
y ==
y if
y >>lambda
y dup
y '[
y ==
y if
y >>list
y dup
y ':
y ==
y if
y >>worddef
y dup
y 'def
y ==
y if
y >>worddef
y dup
y 'var
y ==
y if
y >>var
y dup
y 'del
y ==
y if
y >>del
y dup
y 'true
y ==
y if
y >>true-literal
y dup
y 'false
y ==
y if
y >>false-literal
y dup
y 'null
y ==
y if
y >>null-literal
y dup
i 0
i 3
y slice
y TRIPLEQUOTE
y ref
y ==
y if
y >>triplestring
y dup
i 0
y get
y '"
y ==
y if
y >>string
y dup
y int-symbol?
y if
y >>integer
y dup
y float-symbol?
y if
y >>float
y dup
i 0
y get
y '#
y !=
y if
y >>nomatch
y dup
i 1
i -1
y slice
y float-symbol?
y if
y >>float-literal
y @nomatch
y return
y @void
y return
y @worddef
y drop
y syntax-define-word
y return
y @var
y drop
y syntax-var
y return
y @del
y drop
y syntax-del
y return
y @comment
y drop
y syntax-comment
y syntax-next
y return
y @line-comment
y drop
y syntax-line-comment
y syntax-next
y return
y @lambda
y drop
y syntax-lambda
y return
y @list
y drop
y syntax-list
y return
y @triplestring
y syntax-triple-string
y return
y @string
y syntax-string
y return
y @integer
y parse-int
y return
y @float
y parse-float
y return
y @float-literal
i 1
i -1
y slice
y parse-float
y return
y @true-literal
y drop
b true
y return
y @false-literal
y drop
b false
y return
y @null-literal
y drop
y ,,null
y return
W syntax-define-word
L 41
y syntax-next
y dup
y symbol?
y not
y if
y >>badname
y >L
y SP
y >L
y @loop
y syntax-next
y dup
y void?
y if
y >>eof
y dup
y ';
y ==
y if
y >>endword
y <<loop
y @badname
y str
s Invalid%32name%32after%32':'%32:%32
y swap
y +
y error
y @eof
s Unexpected%32end%32of%32input%32looking%32for%32';'
y error
y @endword
y drop
y L>
y SP
y -
i 1
y -
y make-list
y L>
y save-defined-word
y syntax-next
W syntax-var
L 33
y syntax-next
y dup
y symbol?
y not
y if
y >>badname
y ''
y swap
y +
y syntax-next
y dup
y int?
y not
y if
y >>badcount
y ',,inline
y -rot
y ',,var
i 4
y make-list
y return
y @badname
y str
s Bad%32name%32in%32var:%32
y swap
y +
y error
y @badcount
y str
s Bad%32count%32in%32var:%32
y swap
y +
y error
W syntax-del
L 21
y syntax-next
y dup
y symbol?
y not
y if
y >>badname
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
y >>endstring
y @loop
y reader-next-char
y dup
y void?
y if
y >>eof
y dup
y ord
i 34
y ==
y if
y >>quote
y +
y <<loop
y @quote
y +
y reader-peek-char
y dup
y void?
y if
y >>endquote
y dup
y whitespace?
y if
y >>endquote
y drop
y <<loop
y @endquote
y drop
y >>endstring
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
L 47
y symbol->string
y dup
y length
i 6
y >=
y over
i -3
i -1
y slice
y TRIPLEQUOTE
y ref
y symbol->string
y ==
y and
y if
y >>return
y @read-loop
y reader-next-chunk
y dup
y void?
y if
y >>error-eos
y +
y dup
i -3
i -1
y slice
y TRIPLEQUOTE
y ref
y symbol->string
y ==
y if
y >>return
y <<read-loop
y @error-eos
s String%32ended%32inside%32triple-quoted%32string:%32
y swap
y +
y error
y @return
y dup
y length
i 3
y swap
i 6
y -
y slice
W syntax-lambda
L 22
i 0
y make-list
y @loop
y syntax-next
y dup
y void?
y if
y >>eof
y dup
y '}
y ==
y if
y >>closebrace
y append
y <<loop
y @eof
s Unexpected%32end%32of%32input%32inside%32{%32..%32}
y error
y @closebrace
y drop
y make-lambda
y return
W syntax-list
L 20
i 0
y make-list
y @loop
y syntax-next
y dup
y void?
y if
y >>eof
y dup
y ']
y ==
y if
y >>closelist
y append
y <<loop
y @eof
s Unexpected%32end%32of%32input%32inside%32[%32..%32]
y error
y @closelist
y drop
W syntax-comment
L 73
s 
y >L
i 1
y >L
y @loop
y reader-next
y dup
y void?
y if
y >>eof
y dup
y symbol->string
y L1
y ref
s %32
y +
y swap
y +
y L1
y set!
y dup
y ')
y ==
y if
y >>closeparen
y dup
y '(
y ==
y if
y >>openparen
y drop
y <<loop
y @closeparen
y LP
y ref
i 1
y -
y LP
y set!
y LP
y ref
i 0
y ==
y if
y >>end
y drop
y <<loop
y @openparen
y LP
y ref
i 1
y +
y LP
y set!
y drop
y <<loop
y @end
y LP
i 2
y +
y LP!
y drop
y return
y @eof
y L1
y ref
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
y >>eol
y dup
y ord
i 13
y ==
y if
y >>eol
y dup
y ord
i 10
y ==
y if
y >>eol
y drop
y <<loop
y @eol
y drop
W for-each
L 33
y >L
y dup
y length
y >L
i 0
y >L
y @loop
y L0
y ref
y L1
y ref
y >=
y if
y >>done
y L0
y ref
y +get
y L2
y ref
y call
y L0
y ref
i 1
y +
y L0
y set!
y <<loop
y @done
y drop
y LP
i 3
y +
y LP!
W for-each-reverse
L 32
y >L
y dup
y length
i 1
y -
y >L
y @loop
y L0
y ref
i 0
y <
y if
y >>done
y L0
y ref
y +get
y L1
y ref
y call
y L0
y ref
i 1
y -
y L0
y set!
y <<loop
y @done
y drop
y LP
i 2
y +
y LP!
W serialize-escape-string
L 4
s 
y swap
F
L 84
y unmake
y drop
y dup
i 32
y ==
y if
y >>space
y dup
i 37
y ==
y if
y >>percent
y dup
i 9
y ==
y if
y >>tab
y dup
i 10
y ==
y if
y >>lf
y dup
i 13
y ==
y if
y >>cr
i 1
y make-string
y rot
y swap
y +
y swap
y return
y @space
y drop
y swap
i 37
y chr
s 32
y +
y +
y swap
y return
y @tab
y drop
y swap
i 37
y chr
s 09
y +
y +
y swap
y return
y @percent
y drop
y swap
i 37
y chr
s 37
y +
y +
y swap
y return
y @lf
y drop
y swap
i 37
y chr
y +
s 10
y +
y swap
y return
y @cr
y drop
y swap
i 37
y chr
y +
s 13
y +
y swap
y return
y for-each
W serialize-object
L 96
y dup
y int?
y if
y >>int
y dup
y float?
y if
y >>float
y dup
y string?
y if
y >>string
y dup
y symbol?
y if
y >>symbol
y dup
y list?
y if
y >>list
y dup
y lambda?
y if
y >>lambda
y dup
y bool?
y if
y >>bool
y dup
y null?
y if
y >>null
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
W save-defined-word
L 8
y swap
i 2
y make-list
y COMPILED_NAMELISTS
y ref
y swap
y append
y drop
W byte-compile
L 51
i 0
y make-list
y COMPILED_NAMELISTS
y set!
y SP
y >L
y @loop
y syntax-next
y dup
y void?
y if
y >>done
y dup
y list?
y if
y >>list
y <<loop
y @list
y dup
y length
i 0
y ==
y if
y <<loop
y dup
i 0
y get
y ',,inline
y ==
y if
y >>flatten
y <<loop
y @flatten
i 1
i -1
y slice
y unmake
y drop
y <<loop
y @done
y drop
y L>
y SP
y -
i 1
y -
y make-list
y '__main__
y save-defined-word
y COMPILED_NAMELISTS
y ref
W byte-compile-file
L 2
y reader-open-file
y byte-compile
W byte-compile-string
L 2
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
L 9
y byte-compile-file
s L
y .
y dup
y length
y str
y puts
y CR
y serialize-compiled-output
W compile-and-load-string
L 3
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
W __main__
L 22
y 'READER_TEXT
i 1
y ,,var
y 'READER_TEXT_LEN
i 1
y ,,var
y 'READER_POS
i 1
y ,,var
y 'TRIPLEQUOTE
i 1
y ,,var
i 34
i 34
i 34
i 3
y make-symbol
y TRIPLEQUOTE
y set!
y 'COMPILED_NAMELISTS
i 1
y ,,var
