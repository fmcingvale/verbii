L 74
W depth
L 6
y SP_EMPTY
y ref
y SP
y -
i 1
y -
W .
L 4
y str
y puts
i 32
y .c
W CR
L 2
i 10
y .c
W clear
L 6
y SP
y depth
y +
i 1
y -
y SP!
W drop
L 4
y SP
i 1
y +
y SP!
W dup
L 2
y SP
y ref
W dup?
L 7
y dup
i 0
y ==
y if
y >>ZERO
y dup
y @ZERO
W swap
L 6
y over
y >L
y >L
y drop
y L>
y L>
W pick
L 3
y SP
y +
y ref
W over
L 4
y SP
i 1
y +
y ref
W 2dup
L 2
y over
y over
W 2drop
L 4
y SP
i 2
y +
y SP!
W 2over
L 4
i 4
y pick
i 4
y pick
W 2swap
L 22
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
W ndrop
L 5
y SP
y +
i 1
y +
y SP!
W nip
L 3
y >L
y drop
y L>
W tuck
L 3
y swap
i 2
y pick
W rot
L 9
i 3
y pick
y >L
y >L
y >L
y drop
y L>
y L>
y L>
W -rot
L 12
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
W not
L 6
y if
y >>true
b true
y return
y @true
b false
W or
L 10
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
W and
L 12
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
W !=
L 2
y ==
y not
W >=
L 5
y 2dup
y >
y -rot
y ==
y or
W <
L 2
y swap
y >
W <=
L 2
y >
y not
W neg
L 12
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
W max
L 8
y 2dup
y >
y if
y >>A
y nip
y return
y @A
y drop
W min
L 8
y 2dup
y >
y if
y >>B
y drop
y return
y @B
y nip
W abs
L 8
y dup
i 0
y <
y if
y >>neg
y return
y @neg
y neg
W //
L 3
y /mod
y swap
y drop
W mod
L 2
y /mod
y drop
W ::
L 1
y make-closure
W .S
L 27
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
W .L
L 29
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
W ,,var
L 5
y alloc
i 1
y make-list
y swap
y make-word
W L0
L 1
y LP
W L1
L 3
y LP
i 1
y +
W L2
L 3
y LP
i 2
y +
W L3
L 3
y LP
i 3
y +
W L4
L 3
y LP
i 4
y +
W L5
L 3
y LP
i 5
y +
W L6
L 3
y LP
i 6
y +
W L7
L 3
y LP
i 7
y +
W L8
L 3
y LP
i 8
y +
W L9
L 3
y LP
i 9
y +
W +get
L 3
y over
y swap
y get
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
L 1
y any
W all?
L 1
y all
W whitespace?
L 5
i 0
y get
y ord
i 32
y <=
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
L 4
y string-ltrim
y length
i 0
y ==
W string->symbol
L 2
y unmake
y make-symbol
W symbol->string
L 2
y unmake
y make-string
W ord
L 4
i 0
y get
y unmake
y drop
W chr
L 2
i 1
y make-string
W repeat
L 24
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
W fold
L 18
y swap
i 1
y make-list
L 4
y self
y -rot
y call
y self!
y +
y swap
y ::
y dup
y >L
y for-each
y L0
y ref
y unmake
y nip
y LP
i 1
y +
y LP!
W map
L 42
y over
y list?
y if
y >>map-list
y over
y string?
y if
y >>map-string
y str
s Bad%32sequence%32in%32map:%32
y swap
y +
y error
y @map-string
y '+
y >L
s 
y >L
y >>run
y @map-list
y 'append
y >L
i 0
y make-list
y >L
y @run
L 3
y FUNC
y call
y CONCAT
i 0
y rot
y put
i 2
y L1
y ref
y put
y L0
y ref
y fold
y LP
i 2
y +
y LP!
y return
W filter
L 42
y over
y list?
y if
y >>filter-list
y over
y string?
y if
y >>filter-string
y str
s Bad%32sequence%32in%32filter:%32
y swap
y +
y error
y @filter-string
y '+
y >L
s 
y >L
y >>run
y @filter-list
y 'append
y >L
i 0
y make-list
y >L
y @run
L 9
y dup
y FUNC
y call
y if
y >>keep
y drop
y return
y @keep
y CONCAT
i 1
y rot
y put
i 8
y L1
y ref
y put
y L0
y ref
y fold
y LP
i 2
y +
y LP!
y return
W make-dict
L 3
F
L 3
y unmake
y drop
y put
y ,,new-dict
y fold
W debug
L 3
y .
y .S
y CR
W stream-reader-open-string
L 3
i 0
i 2
y make-list
W stream-reader-open-file
L 2
y read-file
y stream-reader-open-string
W stream-reader-peek-char
L 20
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
W stream-reader-next-char
L 30
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
W stream-reader-skip-whitespace
L 17
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
W stream-reader-next-word
L 28
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
W stream-reader-next-line
L 77
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
W __main__
L 6
y 'SP_EMPTY
i 1
y ,,var
y SP
y SP_EMPTY
y set!
