L 55
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
W true
L 3
i 1
i 1
y ==
W false
L 3
i 1
i 0
y ==
W not
L 6
y if
y >>true
y true
y return
y @true
y false
W or
L 10
y if
y >>true1
y if
y >>true2
y false
y return
y @true1
y drop
y @true2
y true
W and
L 12
y if
y >>true1
y drop
y false
y return
y @true1
y if
y >>true2
y false
y return
y @true2
y true
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
W get
L 26
y over
y list?
y if
y >>list
y over
y string?
y if
y >>string
y over
y symbol?
y if
y >>string
y repr
s Bad%32object%32in%32get:%32
y swap
y +
y error
y @string
i 1
y slice
y return
y @list
i 1
y slice
y unmake
y drop
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
y true
y return
y @noneFound
y drop
y LP
i 3
y +
y LP!
y false
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
y false
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
y true
y return
W tostring
L 6
y dup
y string?
y if
y >>string
y repr
y @string
W string->symbol
L 2
y unmake
y make-symbol
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
W __main__
L 6
y var
y SP_EMPTY
i 1
y SP
y SP_EMPTY
y set!
