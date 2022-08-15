L 80
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
L 4
o 65537
o 1
o 65536
o 0
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
L 2
o 65537
o 1
W 2over
L 8
y SP
i 3
y +
y ref
y SP
i 3
y +
y ref
W 2swap
L 8
o 196609
o 131073
o 65537
o 1
o 131072
o 196608
o 0
o 65536
W ndrop
L 5
y SP
y +
i 1
y +
y SP!
W nip
L 3
o 65537
o 1
o 65536
W tuck
L 5
o 65537
o 1
o 65536
o 0
o 65536
W rot
L 6
o 131073
o 65537
o 1
o 65536
o 131072
o 0
W -rot
L 6
o 131073
o 65537
o 1
o 131072
o 0
o 65536
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
L 9
o 65537
o 1
o 0
o 65536
y >
o 0
o 65536
y ==
y or
W <
L 5
o 65537
o 1
o 65536
o 0
y >
W <=
L 2
y >
y not
W neg
L 4
o 1
i 0
o 0
y -
W max
L 11
o 65537
o 1
o 0
o 65536
y >
y if
y >>A
o 65536
y return
y @A
o 0
W min
L 11
o 65537
o 1
o 0
o 65536
y >
y if
y >>B
o 0
y return
y @B
o 65536
W abs
L 11
o 1
o 0
i 0
y <
y if
y >>neg
o 0
y return
y @neg
o 0
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
W +get
L 3
y over
y swap
y get
W any?
L 29
o 65537
o 1
i 0
o 196609
y @loop
o 196608
o 0
y length
y >=
y if
y >>noneFound
o 0
o 196608
y get
o 65536
y call
y if
y >>gotTrue
o 196608
i 1
y +
o 196609
y <<loop
y @gotTrue
b true
y return
y @noneFound
b false
y return
W all?
L 29
o 65537
o 1
i 0
o 196609
y @loop
o 196608
o 0
y length
y >=
y if
y >>allTrue
o 0
o 196608
y get
o 65536
y call
y if
y >>gotTrue
b false
y return
y @gotTrue
o 196608
i 1
y +
o 196609
y <<loop
y @allTrue
b true
y return
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
W string-join
L 7
o 65537
o 1
o 0
F
L 17
o 65537
o 1
o 0
y length
i 0
y ==
y if
y >>skip
o 0
o 65792
y +
o 65536
y +
y return
y @skip
o 65536
y return
y bind-lambda
s 
y fold
W string->symbol
L 2
y unmake
y make-symbol
W symbol->string
L 1
y str
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
W upper?
L 10
o 1
o 0
y ord
i 65
y >=
o 0
y ord
i 90
y <=
y and
W lower?
L 10
o 1
o 0
y ord
i 97
y >=
o 0
y ord
i 122
y <=
y and
W tolower
L 15
o 1
o 0
y upper?
y if
y >>is-upper
o 0
y return
y @is-upper
o 0
y ord
i 65
y -
i 97
y +
y chr
W toupper
L 15
o 1
o 0
y lower?
y if
y >>is-lower
o 0
y return
y @is-lower
o 0
y ord
i 97
y -
i 65
y +
y chr
W string-lower
L 4
o 1
o 0
F
L 1
y tolower
y map
W string-upper
L 4
o 1
o 0
F
L 1
y toupper
y map
W string-split
L 65
o 65537
o 1
L 0
o 131073
i 0
o 196609
s 
o 327681
y @loop
o 0
o 196608
y get
o 262145
o 262144
y void?
y if
y >>got-end
o 65536
F
L 2
o 262400
y ==
y bind-lambda
y find
y void?
y not
y if
y >>got-delim
o 327680
o 262144
y +
o 327681
y >>next
y @got-delim
o 327680
y length
i 0
y >
y if
y >>push-piece
y >>next
y @push-piece
o 131072
o 327680
y append
o 131073
s 
o 327681
y >>next
y @next
o 196608
i 1
y +
o 196609
y <<loop
y @got-end
o 327680
y length
i 0
y ==
y if
y >>done
o 131072
o 327680
y append
o 131073
y @done
o 131072
W ifelse
L 12
o 131073
o 65537
o 1
o 0
y if
y >>true
o 131072
y call
y return
y @true
o 65536
y call
W ifthen
L 9
o 65537
o 1
o 0
y if
y >>true
y return
y @true
o 65536
y call
W repeat
L 16
o 65537
o 1
y @loop
o 65536
i 0
y <=
y if
y >>done
o 0
y call
o 65536
i 1
y -
o 65537
y <<loop
y @done
W for-each
L 22
o 65537
o 1
i 0
o 131073
y @loop
o 131072
o 0
y length
y >=
y if
y >>done
o 0
o 131072
y get
o 65536
y call
o 131072
i 1
y +
o 131073
y <<loop
y @done
W for-each-reverse
L 24
o 65537
o 1
o 0
y length
i 1
y -
o 131073
y @loop
o 131072
i 0
y <
y if
y >>done
o 0
o 131072
y get
o 65536
y call
o 131072
i 1
y -
o 131073
y <<loop
y @done
W for-each-i
L 23
o 65537
o 1
i 0
o 131073
y @loop
o 131072
o 0
y length
y >=
y if
y >>done
o 131072
o 0
o 131072
y get
o 65536
y call
o 131072
i 1
y +
o 131073
y <<loop
y @done
W for-each-dict
L 29
o 65537
o 1
o 0
y keys
o 196609
i 0
o 131073
y @loop
o 131072
o 196608
y length
y >=
y if
y >>done
o 196608
o 131072
y get
y dup
o 0
y swap
y get
o 65536
y call
o 131072
i 1
y +
o 131073
y <<loop
y @done
W for-range
L 27
o 196609
o 131073
o 65537
o 1
i 0
o 262145
y @loop
o 196608
i 0
y <=
y if
y >>done
o 262144
o 65536
y +
o 0
y call
o 262144
o 131072
y +
o 262145
o 196608
i 1
y -
o 196609
y <<loop
y @done
W find
L 28
o 65537
o 1
i 0
o 131073
y @loop
o 131072
o 0
y length
y >=
y if
y >>not-found
o 0
o 131072
y get
o 65536
y call
y if
y >>found
o 131072
i 1
y +
o 131073
y <<loop
y @found
o 131072
y return
y @not-found
y void
W fold
L 28
o 131073
o 65537
o 1
o 131072
o 196609
i 0
o 262145
y @loop
o 262144
o 0
y length
y >=
y if
y >>done
o 196608
o 0
o 262144
y get
o 65536
y call
o 196609
o 262144
i 1
y +
o 262145
y <<loop
y @done
o 196608
W map-list
L 29
o 65537
o 1
i 0
o 196609
L 0
o 131073
y @loop
o 196608
o 0
y length
y >=
y if
y >>done
o 0
o 196608
y get
o 65536
y call
o 131072
y swap
y append
o 131073
o 196608
i 1
y +
o 196609
y <<loop
y @done
o 131072
W map-string
L 29
o 65537
o 1
i 0
o 196609
s 
o 131073
y @loop
o 196608
o 0
y length
y >=
y if
y >>done
o 0
o 196608
y get
o 65536
y call
o 131072
y swap
y +
o 131073
o 196608
i 1
y +
o 196609
y <<loop
y @done
o 131072
W map
L 25
o 65537
o 1
o 0
y list?
y if
y >>list
o 0
y string?
y if
y >>string
s Bad%32sequence%32in%32map:
o 0
y str
y +
y error
y @list
o 0
o 65536
y map-list
y return
y @string
o 0
o 65536
y map-string
y return
W filter-list
L 35
o 65537
o 1
L 0
o 131073
i 0
o 196609
y @loop
o 196608
o 0
y length
y >=
y if
y >>done
o 0
o 196608
y get
o 65536
y call
y not
y if
y >>next
o 131072
o 0
o 196608
y get
y append
o 131073
y @next
o 196608
i 1
y +
o 196609
y <<loop
y @done
o 131072
W filter-string
L 35
o 65537
o 1
s 
o 131073
i 0
o 196609
y @loop
o 196608
o 0
y length
y >=
y if
y >>done
o 0
o 196608
y get
o 65536
y call
y not
y if
y >>next
o 131072
o 0
o 196608
y get
y +
o 131073
y @next
o 196608
i 1
y +
o 196609
y <<loop
y @done
o 131072
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
L 5
y '#stream
y swap
i 0
i 3
y make-list
W stream-reader-open-file
L 2
y file-read
y stream-reader-open-string
W stream-reader-tell
L 5
o 1
o 0
o 0
i 2
y get
W stream-reader-seek
L 6
o 65537
o 1
o 0
i 2
o 65536
y put
W stream-reader-peek-char
L 12
o 1
o 0
i 2
y get
o 131073
o 0
y stream-reader-next-char
o 65537
i 2
o 131072
y put
o 65536
W stream-reader-next-char
L 33
o 1
o 0
i 2
y get
o 0
i 1
y get
y length
y >=
y if
y >>eof
o 0
i 1
y get
o 0
i 2
y get
y get
o 65537
o 0
y dup
i 2
y get
i 1
y +
i 2
y swap
y put
o 65536
y return
y @eof
o 0
y void
W stream-reader-skip-whitespace
L 18
o 1
o 0
y @loop
y stream-reader-peek-char
o 65537
o 65536
y void?
y if
y >>done
o 65536
y whitespace?
y not
y if
y >>done
y stream-reader-next-char
y drop
y <<loop
y @done
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
W stream-reader-peek-word
L 12
o 1
o 0
i 2
y get
o 131073
o 0
y stream-reader-next-word
o 65537
i 2
o 131072
y put
o 65536
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
W stream-reader-peek-line
L 12
o 1
o 0
i 2
y get
o 131073
o 0
y stream-reader-next-line
o 65537
i 2
o 131072
y put
o 65536
W __main__
L 0
