L 9
W safe-run-and-delete-main
L 6
y '__main__
y .dumpword
y '__main__
y ,,del
y call
y return
W deserialize-and-run
L 3
y deserialize
y safe-run-and-delete-main
y return
W init-interpreter
L 15
s ../lib/init.verb.b
y deserialize-and-run
s ../lib/compiler.verb.b
y deserialize-and-run
s ../lib/patches.verb
y file-read
b true
y set-allow-overwrite-words
y compile-and-load-string
y __main__
y '__main__
y ,,del
b false
y set-allow-overwrite-words
y return
W repl
L 23
b false
y set-exit-on-exception
s Verbii%32(mini)
y .
y CR
y @loop
s >>%32
y prompt
y dup
y void?
y if
y >>eof
y compile-and-load-string
y safe-run-and-delete-main
y .S
y <<loop
y @eof
y drop
y CR
s Exiting%32on%32EOF
y .
y CR
y return
W load-file-maybe-cached
L 10
y NOCACHE
y ref
y if
y >>nocache
y cached-compile-and-load
y return
y @nocache
y file-read
y compile-and-load-string
y return
W import
L 32
y IMPORTED_MODULES
y ref
y over
y get
y void?
y not
y if
y >>already-imported
y dup
y IMPORTED_MODULES
y ref
y swap
b true
y put
y drop
s .verb
y +
y dup
y file-exists?
y if
y >>good
s Unable%32to%32find%32file:
y swap
y +
y error
y @good
y load-file-maybe-cached
y safe-run-and-delete-main
y return
y @already-imported
y drop
y return
W run-file
L 9
b true
y set-exit-on-exception
y load-file-maybe-cached
y '__main__
y .dumpword
y '__main__
y ,,del
y call
y return
W run-test
L 31
y stream-reader-open-file
y TESTMODE_STREAM
y set!
y @loop
y TESTMODE_STREAM
y ref
y stream-reader-next-line
y nip
y dup
y void?
y if
y >>done
y dup
y string-empty?
y if
y >>skip
y dup
s >>
y .
y .
y CR
y compile-and-load-string
y safe-run-and-delete-main
y .S
y <<loop
y @skip
y drop
y <<loop
y @done
y drop
y return
W __main__
L 63
y init-interpreter
y 'TESTMODE_STREAM
i 1
y ,,var
y 'IMPORTED_MODULES
i 1
y ,,var
y ,,new-dict
y IMPORTED_MODULES
y set!
y 'FILENAME
i 1
y ,,var
y void
y FILENAME
y set!
y 'NOCACHE
i 1
y ,,var
b false
y NOCACHE
y set!
y 'TESTMODE
i 1
y ,,var
b false
y TESTMODE
y set!
y 'BADARGS
i 1
y ,,var
b false
y BADARGS
y set!
y cmdline-args
F
L 55
y dup
s -nocache
y ==
y if
y >>set-nocache
y dup
s -test
y ==
y if
y >>set-testmode
y dup
y file-exists?
y if
y >>set-filename
s Unrecognized%32command%32line%32argument:%32
y swap
y +
y .
y CR
b true
y BADARGS
y set!
y return
y @set-nocache
y drop
b true
y NOCACHE
y set!
y return
y @set-testmode
y drop
b true
y TESTMODE
y set!
y return
y @set-filename
y FILENAME
y ref
y void?
y not
y if
y >>have-filename
y FILENAME
y set!
y return
y @have-filename
s Extra%32filename%32given%32on%32command%32line:%32
y swap
y +
y .
y CR
b true
y BADARGS
y set!
y return
y for-each
y BADARGS
y ref
y if
y >>exit
y FILENAME
y ref
y void?
y if
y >>do-repl
y TESTMODE
y ref
y if
y >>do-test-mode
y FILENAME
y ref
y run-file
y >>exit
y @do-test-mode
y FILENAME
y ref
y run-test
y >>exit
y @do-repl
y repl
y @exit
y return
