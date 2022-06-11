L 81
W depth
L 7
y SP_EMPTY
y ref
y SP
y -
i 1
y -
y return
W .
L 5
y str
y puts
i 32
y .c
y return
W CR
L 3
i 10
y .c
y return
W clear
L 7
y SP
y depth
y +
i 1
y -
y SP!
y return
W drop
L 5
y SP
i 1
y +
y SP!
y return
W dup
L 3
y SP
y ref
y return
W dup?
L 8
y dup
i 0
y ==
y if
y >>ZERO
y dup
y @ZERO
y return
W swap
L 7
y over
y >L
y >L
y drop
y L>
y L>
y return
W pick
L 4
y SP
y +
y ref
y return
W over
L 5
y SP
i 1
y +
y ref
y return
W 2dup
L 3
y over
y over
y return
W 2drop
L 5
y SP
i 2
y +
y SP!
y return
W 2over
L 5
i 4
y pick
i 4
y pick
y return
W 2swap
L 23
y >L
y >L
y >L
y >L
y LP
i 2
y +
y ref
y LP
i 3
y +
y ref
y LP
y ref
y LP
i 1
y +
y ref
y LP
i 4
y +
y LP!
y return
W ndrop
L 6
y SP
y +
i 1
y +
y SP!
y return
W nip
L 4
y >L
y drop
y L>
y return
W tuck
L 4
y swap
i 2
y pick
y return
W rot
L 10
i 3
y pick
y >L
y >L
y >L
y drop
y L>
y L>
y L>
y return
W -rot
L 13
i 2
y pick
y >L
i 3
y pick
y >L
y >L
y drop
y drop
y L>
y L>
y L>
y return
W not
L 7
y if
y >>true
b true
y return
y @true
b false
y return
W or
L 11
y if
y >>true1
y if
y >>true2
b false
y return
y @true1
y drop
y @true2
b true
y return
W and
L 13
y if
y >>true1
y drop
b false
y return
y @true1
y if
y >>true2
b false
y return
y @true2
b true
y return
W !=
L 3
y ==
y not
y return
W >=
L 6
y 2dup
y >
y -rot
y ==
y or
y return
W <
L 3
y swap
y >
y return
W <=
L 3
y >
y not
y return
W neg
L 13
y dup
y int?
y if
y >>int
i 0
y swap
y -
y return
y @int
i 0
y swap
y -
y return
W max
L 9
y 2dup
y >
y if
y >>A
y nip
y return
y @A
y drop
y return
W min
L 9
y 2dup
y >
y if
y >>B
y drop
y return
y @B
y nip
y return
W abs
L 9
y dup
i 0
y <
y if
y >>neg
y return
y @neg
y neg
y return
W //
L 4
y /mod
y swap
y drop
y return
W mod
L 3
y /mod
y drop
y return
W ::
L 2
y make-closure
y return
W .S
L 28
s =>
y .
y depth
y @LOOP
y dup
i 0
y >
y if
y >>true
y >>done
y @true
y dup
y SP
y +
i 1
y +
y ref
y repr
y puts
i 32
y .c
i 1
y -
y <<LOOP
y @done
y drop
y CR
y return
W .L
L 30
i 0
s Locals:
y .
y @loop
y 2dup
y ==
y if
y >>done
y dup
y str
s L
y swap
y +
s :
y +
y .
y dup
y LP
y +
y ref
y repr
y puts
i 32
y .c
i 1
y +
y <<loop
y @done
y 2drop
y return
W ,,var
L 6
y alloc
i 1
y make-list
y swap
y make-word
y return
W L0
L 2
y LP
y return
W L1
L 4
y LP
i 1
y +
y return
W L2
L 4
y LP
i 2
y +
y return
W L3
L 4
y LP
i 3
y +
y return
W L4
L 4
y LP
i 4
y +
y return
W L5
L 4
y LP
i 5
y +
y return
W L6
L 4
y LP
i 6
y +
y return
W L7
L 4
y LP
i 7
y +
y return
W L8
L 4
y LP
i 8
y +
y return
W L9
L 4
y LP
i 9
y +
y return
W +get
L 4
y over
y swap
y get
y return
W any
L 45
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
y ==
y if
y >>noneFound
y L0
y ref
y +get
y L2
y ref
y call
y if
y >>gotTrue
y L0
y ref
i 1
y +
y L0
y set!
y <<loop
y @gotTrue
y drop
y LP
i 3
y +
y LP!
b true
y return
y @noneFound
y drop
y LP
i 3
y +
y LP!
b false
y return
W all
L 45
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
y ==
y if
y >>allTrue
y L0
y ref
y +get
y L2
y ref
y call
y if
y >>gotTrue
y drop
y LP
i 3
y +
y LP!
b false
y return
y @gotTrue
y L0
y ref
i 1
y +
y L0
y set!
y <<loop
y @allTrue
y drop
y LP
i 3
y +
y LP!
b true
y return
W any?
L 2
y any
y return
W all?
L 2
y all
y return
W file-read
L 2
y read-file
y return
W whitespace?
L 6
i 0
y get
y ord
i 32
y <=
y return
W string-ltrim
L 26
i 0
y @loop
y 2dup
y swap
y length
y >=
y if
y >>empty
y 2dup
y get
y whitespace?
y not
y if
y >>endtrim
i 1
y +
y <<loop
y @endtrim
i -1
y slice
y return
y @empty
y drop
y drop
s 
y return
W string-empty?
L 5
y string-ltrim
y length
i 0
y ==
y return
W string-join
L 23
y LP
i 0
y -
y LP!
y >L
y >L
y LP
i 0
y +
y ref
F
L 12
y over
y length
i 0
y ==
y if
y >>skip
y swap
y self
y +
y swap
y @skip
y +
y LP
i 1
y +
y ref
y ::
s 
y fold
y LP
i 2
y +
y LP!
y return
W string->symbol
L 3
y unmake
y make-symbol
y return
W symbol->string
L 3
y unmake
y make-string
y return
W ord
L 5
i 0
y get
y unmake
y drop
y return
W chr
L 3
i 1
y make-string
y return
W repeat
L 25
y >L
y >L
y @loop
y L1
y ref
i 0
y ==
y if
y >>done
y L0
y ref
y call
y L1
y ref
i 1
y -
y L1
y set!
y <<loop
y @done
y LP
i 2
y +
y LP!
y return
W for-each
L 55
y LP
i 1
y -
y LP!
y >L
y >L
i 0
y LP
i 2
y +
y set!
y @loop
y LP
i 2
y +
y ref
y LP
i 0
y +
y ref
y length
y >=
y if
y >>done
y LP
i 0
y +
y ref
y LP
i 2
y +
y ref
y get
y LP
i 1
y +
y ref
y call
y LP
i 2
y +
y ref
i 1
y +
y LP
i 2
y +
y set!
y <<loop
y @done
y LP
i 3
y +
y LP!
y return
W for-each-reverse
L 57
y LP
i 1
y -
y LP!
y >L
y >L
y LP
i 0
y +
y ref
y length
i 1
y -
y LP
i 2
y +
y set!
y @loop
y LP
i 2
y +
y ref
i 0
y <
y if
y >>done
y LP
i 0
y +
y ref
y LP
i 2
y +
y ref
y get
y LP
i 1
y +
y ref
y call
y LP
i 2
y +
y ref
i 1
y -
y LP
i 2
y +
y set!
y <<loop
y @done
y LP
i 3
y +
y LP!
y return
W for-each-i
L 59
y LP
i 1
y -
y LP!
y >L
y >L
i 0
y LP
i 2
y +
y set!
y @loop
y LP
i 2
y +
y ref
y LP
i 0
y +
y ref
y length
y >=
y if
y >>done
y LP
i 2
y +
y ref
y LP
i 0
y +
y ref
y LP
i 2
y +
y ref
y get
y LP
i 1
y +
y ref
y call
y LP
i 2
y +
y ref
i 1
y +
y LP
i 2
y +
y set!
y <<loop
y @done
y LP
i 3
y +
y LP!
y return
W fold
L 76
y LP
i 2
y -
y LP!
y >L
y >L
y >L
y LP
i 2
y +
y ref
y LP
i 3
y +
y set!
i 0
y LP
i 4
y +
y set!
y @loop
y LP
i 4
y +
y ref
y LP
i 0
y +
y ref
y length
y >=
y if
y >>done
y LP
i 3
y +
y ref
y LP
i 0
y +
y ref
y LP
i 4
y +
y ref
y get
y LP
i 1
y +
y ref
y call
y LP
i 3
y +
y set!
y LP
i 4
y +
y ref
i 1
y +
y LP
i 4
y +
y set!
y <<loop
y @done
y LP
i 3
y +
y ref
y LP
i 5
y +
y LP!
y return
W map-list
L 74
y LP
i 2
y -
y LP!
y >L
y >L
i 0
y LP
i 2
y +
y set!
L 0
y LP
i 3
y +
y set!
y @loop
y LP
i 2
y +
y ref
y LP
i 0
y +
y ref
y length
y >=
y if
y >>done
y LP
i 0
y +
y ref
y LP
i 2
y +
y ref
y get
y LP
i 1
y +
y ref
y call
y LP
i 3
y +
y ref
y swap
y append
y LP
i 3
y +
y set!
y LP
i 2
y +
y ref
i 1
y +
y LP
i 2
y +
y set!
y <<loop
y @done
y LP
i 3
y +
y ref
y LP
i 4
y +
y LP!
y return
W map-string
L 74
y LP
i 2
y -
y LP!
y >L
y >L
i 0
y LP
i 2
y +
y set!
s 
y LP
i 3
y +
y set!
y @loop
y LP
i 2
y +
y ref
y LP
i 0
y +
y ref
y length
y >=
y if
y >>done
y LP
i 0
y +
y ref
y LP
i 2
y +
y ref
y get
y LP
i 1
y +
y ref
y call
y LP
i 3
y +
y ref
y swap
y +
y LP
i 3
y +
y set!
y LP
i 2
y +
y ref
i 1
y +
y LP
i 2
y +
y set!
y <<loop
y @done
y LP
i 3
y +
y ref
y LP
i 4
y +
y LP!
y return
W map
L 58
y LP
i 0
y -
y LP!
y >L
y >L
y LP
i 0
y +
y ref
y list?
y if
y >>list
y LP
i 0
y +
y ref
y string?
y if
y >>string
s Bad%32sequence%32in%32map:
y LP
i 0
y +
y ref
y str
y +
y error
y @list
y LP
i 0
y +
y ref
y LP
i 1
y +
y ref
y map-list
y LP
i 2
y +
y LP!
y return
y @string
y LP
i 0
y +
y ref
y LP
i 1
y +
y ref
y map-string
y LP
i 2
y +
y LP!
y return
W filter-list
L 86
y LP
i 2
y -
y LP!
y >L
y >L
L 0
y LP
i 2
y +
y set!
i 0
y LP
i 3
y +
y set!
y @loop
y LP
i 3
y +
y ref
y LP
i 0
y +
y ref
y length
y >=
y if
y >>done
y LP
i 0
y +
y ref
y LP
i 3
y +
y ref
y get
y LP
i 1
y +
y ref
y call
y not
y if
y >>next
y LP
i 2
y +
y ref
y LP
i 0
y +
y ref
y LP
i 3
y +
y ref
y get
y append
y LP
i 2
y +
y set!
y @next
y LP
i 3
y +
y ref
i 1
y +
y LP
i 3
y +
y set!
y <<loop
y @done
y LP
i 2
y +
y ref
y LP
i 4
y +
y LP!
y return
W filter-string
L 86
y LP
i 2
y -
y LP!
y >L
y >L
s 
y LP
i 2
y +
y set!
i 0
y LP
i 3
y +
y set!
y @loop
y LP
i 3
y +
y ref
y LP
i 0
y +
y ref
y length
y >=
y if
y >>done
y LP
i 0
y +
y ref
y LP
i 3
y +
y ref
y get
y LP
i 1
y +
y ref
y call
y not
y if
y >>next
y LP
i 2
y +
y ref
y LP
i 0
y +
y ref
y LP
i 3
y +
y ref
y get
y +
y LP
i 2
y +
y set!
y @next
y LP
i 3
y +
y ref
i 1
y +
y LP
i 3
y +
y set!
y <<loop
y @done
y LP
i 2
y +
y ref
y LP
i 4
y +
y LP!
y return
W filter
L 20
y over
y list?
y if
y >>list
y over
y string?
y if
y >>string
y drop
s Bad%32sequence%32in%32filter:%32
y swap
y str
y +
y error
y @list
y filter-list
y return
y @string
y filter-string
y return
W make-dict
L 4
F
L 3
y unmake
y drop
y put
y ,,new-dict
y fold
y return
W debug
L 4
y .
y .S
y CR
y return
W stream-reader-open-string
L 4
i 0
i 2
y make-list
y return
W stream-reader-open-file
L 3
y read-file
y stream-reader-open-string
y return
W stream-reader-peek-char
L 21
y dup
i 1
y get
y over
i 0
y get
y length
y >=
y if
y >>eof
y dup
i 0
y get
y over
i 1
y get
y get
y return
y @eof
y void
y return
W stream-reader-next-char
L 31
y dup
i 1
y get
y over
i 0
y get
y length
y >=
y if
y >>eof
y dup
i 0
y get
y over
i 1
y get
y get
y swap
y dup
i 1
y get
i 1
y +
i 1
y swap
y put
y swap
y return
y @eof
y void
y return
W stream-reader-skip-whitespace
L 18
y @loop
y stream-reader-peek-char
y dup
y void?
y if
y >>done
y dup
y whitespace?
y not
y if
y >>done
y drop
y stream-reader-next-char
y drop
y <<loop
y @done
y drop
y return
W stream-reader-next-word
L 29
y stream-reader-skip-whitespace
s 
y @loop
y over
y stream-reader-next-char
y nip
y dup
y void?
y if
y >>done
y dup
y whitespace?
y if
y >>done
y +
y <<loop
y @done
y drop
y dup
y length
i 0
y ==
y if
y >>void
y return
y @void
y drop
y void
y return
W stream-reader-next-line
L 78
s 
y @loop
y over
y stream-reader-next-char
y nip
y dup
y void?
y if
y >>eof
y dup
y ord
i 10
y ==
y if
y >>NL
y dup
y ord
i 13
y ==
y if
y >>CR
y +
y <<loop
y @NL
y drop
y over
y stream-reader-peek-char
y nip
y dup
y void?
y if
y >>drop-return
y dup
y ord
i 13
y ==
y if
y >>skip-next
y >>drop-return
y @CR
y drop
y over
y stream-reader-peek-char
y nip
y dup
y void?
y if
y >>drop-return
y dup
y ord
i 10
y ==
y if
y >>skip-next
y >>drop-return
y @drop-return
y drop
y return
y @skip-next
y drop
y over
y stream-reader-next-char
y drop
y drop
y return
y @eof
y drop
y dup
y length
i 0
y ==
y if
y >>void
y return
y @void
y drop
y void
y return
W __main__
L 7
y 'SP_EMPTY
i 1
y ,,var
y SP
y SP_EMPTY
y set!
y return
