L 55
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
L 11
y READER-POS
y READER-TEXT-LEN
y >=
y if
y >>eof
y READER-TEXT
y READER-POS
y get
y return
y @eof
y void
W reader-next-char
L 15
y READER-POS
y READER-TEXT-LEN
y >=
y if
y >>eof
y READER-TEXT
y READER-POS
y get
y READER-POS
i 1
y +
y READER-POS!
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
L 32
o 1
o 0
y string?
y if
y >>string
o 0
y '0
y <
y if
y >>notdigit
o 0
y '9
y >
y if
y >>notdigit
b true
y return
y @string
o 0
s 0
y <
y if
y >>notdigit
o 0
s 9
y >
y if
y >>notdigit
b true
y return
y @notdigit
b false
W any-digits?
L 4
o 1
o 0
F
L 1
y digit?
y any?
W ltrim-char
L 16
o 65537
o 1
o 0
i 0
y get
o 65536
y ==
y if
y >>trim
o 0
y return
y @trim
o 0
i 1
i -1
y slice
W ltrim-sign
L 22
o 1
o 0
i 0
y get
s +
y ==
y if
y >>trim
o 0
i 0
y get
s -
y ==
y if
y >>trim
o 0
y return
y @trim
o 0
i 1
i -1
y slice
W ltrim-digits
L 23
o 1
y @loop
o 0
i 0
y get
y void?
y if
y >>done
o 0
i 0
y get
y digit?
y not
y if
y >>done
o 0
i 1
i -1
y slice
o 1
y <<loop
y @done
o 0
W int-string?
L 15
o 1
o 0
y any-digits?
y not
y if
y >>nodigits
o 0
y ltrim-sign
y ltrim-digits
y length
i 0
y ==
y return
y @nodigits
b false
W int-symbol?
L 4
o 1
o 0
y str
y int-string?
W float-string?
L 24
o 1
o 0
y any-digits?
y not
y if
y >>nodigits
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
y @nodigits
b false
W float-symbol?
L 2
y str
y float-string?
W dict-add-indexed-names
L 27
o 65537
o 1
i 0
o 196609
y @loop
o 196608
o 65536
y length
y >=
y if
y >>done
o 0
o 65536
o 196608
y get
y str
o 0
y length
y put
y drop
o 196608
i 1
y +
o 196609
y <<loop
y @done
o 0
W syntax-next
L 166
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
y >>v2-closure
y dup
y '::{
y ==
y if
y >>v2-closure
y dup
y '[
y ==
y if
y >>list
y dup
y '#op(
y ==
y if
y >>opcode
y dup
y ':
y ==
y if
y >>v2-worddef
y dup
y ':V2
y ==
y if
y >>v2-worddef
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
y @v2-worddef
y drop
y v2-syntax-define-word
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
y @v2-closure
y drop
y syntax-v2-closure
y return
y @list
y drop
y syntax-list
y return
y @opcode
y drop
y syntax-opcode
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
n 
y return
W flatten-if-inline
L 22
y dup
y length
i 0
y ==
y if
y >>done
y dup
i 0
y get
y ',,inline
y ==
y if
y >>flatten
y return
y @flatten
i 1
i -1
y slice
y unmake
y drop
y return
y @done
W syntax-parse-decl-arglist
L 27
y ',,declargs
i 1
y make-list
o 1
y @loop
y syntax-next
y dup
y void?
y if
y >>eof
y dup
y ')
y ==
y if
y >>done
o 0
y swap
y append
o 1
y <<loop
y @eof
y drop
s End%32of%32input%32inside%32@(
y error
y @done
y drop
o 0
W v2-syntax-define-word
L 50
y syntax-next
y dup
y symbol?
y not
y if
y >>badname
o 65537
L 0
o 131073
y reader-peek-next
y '(
y ==
y if
y >>read-arglist
y >>loop
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
y >>eof
y dup
y ';
y ==
y if
y >>done
o 131072
y swap
y append
o 131073
y <<loop
y @eof
y drop
s End%32of%32input%32inside%32:W
y error
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
y >>eof
o 65536
y ')
y ==
y if
y >>closelist
o 0
o 65536
y append
o 1
y <<loop
y @eof
s Unexpected%32end%32of%32input%32inside%32#op(%32..%32)
y error
y @closelist
o 0
y length
i 0
y ==
y if
y >>no-name
o 0
y length
i 4
y >
y if
y >>too-long
y @padlist
o 0
y length
i 4
y ==
y if
y >>make-opcode
o 0
s 0
y append
o 1
y <<padlist
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
L 45
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
y >>eof
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
y >>closeparen
y dup
y '(
y ==
y if
y >>openparen
y drop
y <<loop
y @closeparen
o 0
i 1
y -
o 1
o 0
i 0
y ==
y if
y >>end
y drop
y <<loop
y @openparen
o 0
i 1
y +
o 1
y drop
y <<loop
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
W syntax-v2-closure
L 40
L 0
o 1
y reader-peek-next
y '(
y ==
y if
y >>read-arglist
y >>loop
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
y >>eof
y dup
y '}
y ==
y if
y >>done
o 0
y swap
y append
o 1
y <<loop
y @eof
y drop
s End%32of%32input%32inside%32{%32..%32}
y error
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
y >>space
y dup
y ord
i 37
y ==
y if
y >>percent
y dup
y ord
i 9
y ==
y if
y >>tab
y dup
y ord
i 10
y ==
y if
y >>lf
y dup
y ord
i 13
y ==
y if
y >>cr
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
y dup
y opcode?
y if
y >>opcode
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
y >>done
o 0
o 65536
y get
y '--
y ==
y if
y >>got-dashes
o 262144
y if
y >>add-to-locals
o 131072
o 0
o 65536
y get
y append
o 131073
y >>next
y @add-to-locals
o 196608
o 0
o 65536
y get
y append
o 196609
y >>next
y @got-dashes
b true
o 262145
y @next
o 65536
i 1
y +
o 65537
y <<loop
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
y >>done
y dup
y list?
y if
y >>check-list
y dup
y '@args
y ==
y if
y >>args-list
y dup
y '@locals
y ==
y if
y >>locals-list
y >>keep-obj
y @check-list
y dup
y length
i 0
y ==
y if
y >>keep-obj
y dup
i 0
y get
y ',,declargs
y !=
y if
y >>keep-obj
y compile-split-declargs
o 196608
y swap
y +
o 196609
o 131072
y swap
y +
o 131073
y >>next
y @keep-obj
o 65536
y swap
y append
o 65537
y >>next
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
y >>missing-args
y dup
y list?
y not
y if
y >>missing-args
o 131072
y swap
y +
o 131073
y >>next
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
y >>missing-locals
y dup
y list?
y not
y if
y >>missing-locals
o 196608
y swap
y +
o 196609
y >>next
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
y <<loop
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
y >>bad-env
o 0
y closure-env-dict
o 65536
y str
y get
y void?
y not
y if
y >>dup-name
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
y >>lookup
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
y >>try-up
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
y >>not-found
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
y >>set
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
L 186
o 131073
o 65537
o 1
o 0
y compile-collect-args-locals
o 393217
o 327681
o 196609
i 0
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
y for-range
o 131072
y if
y >>make-env-plain-list
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
y >>dup-name
o 459008
o 0
y closure-env-add-name
y @dup-name
y bind-lambda
y for-each
o 458752
o 65536
y closure-env-set-up
y >>begin-rewrite
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
y >>done
o 0
o 524288
y get
y symbol?
y if
y >>symbol
o 0
o 524288
y get
y list?
y if
y >>rewrite-inner-plain-list
o 0
o 524288
y get
y lambda?
y if
y >>rewrite-inner-lambda
y @copy-element
o 196608
o 0
o 524288
y get
y append
o 196609
y >>next
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
y <<copy-element
o 196608
o 720896
o 589824
o 655360
y closure-env-make-ref-or-set
y append
o 196609
o 262144
i 1
y +
o 262145
y >>next
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
y ==
y if
y >>done-rewrite-sub
o 196608
y 'bind-lambda
y append
o 196609
y @done-rewrite-sub
o 262144
o 851968
y +
o 262145
y >>next
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
y ==
y if
y >>plain-list-no-rewrites
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
y +
o 262145
y >>next
y @plain-list-no-rewrites
o 196608
o 0
o 524288
y get
y append
o 196609
y >>next
y @next
o 524288
i 1
y +
o 524289
y <<rewrite-loop
y @done
o 196608
o 262144
y return
W compile-finalize-word
L 7
o 1
o 0
y void
b false
y v2-compile-rewrite-args-locals
y drop
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
y >>done
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
y <<loop
y @done
y drop
W byte-compile
L 31
i 0
y make-list
y COMPILED-NAMELISTS!
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
y flatten-if-inline
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
L 11
o 1
o 0
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
W cached-compile-and-load
L 33
y dup
s .b
y +
y file-exists?
y not
y if
y >>do-compile
y dup
y file-mtime
y over
s .b
y +
y file-mtime
y >
y if
y >>do-compile
s .b
y +
y deserialize
y return
y @do-compile
y dup
s .b
y +
y open-as-stdout
y dup
y compile-and-serialize
y void
y open-as-stdout
s .b
y +
y deserialize
y return
W __main__
L 19
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
y 'COMPILED-NAMELISTS
y void
y make-var
