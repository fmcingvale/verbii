L 6
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
W __main__
L 80
y init-interpreter
y 'IMPORTED_MODULES
i 1
y ,,var
y ,,new-dict
y IMPORTED_MODULES
y set!
y 'NOCACHE
i 1
y ,,var
b false
y NOCACHE
y set!
y 'SCRIPT_ARGS
i 1
y ,,var
L 0
y >L
i 0
y @parse-cmdline
y dup
y cmdline-args
y length
y >=
y if
y >>done
y cmdline-args
y over
y get
y dup
s --
y ==
y if
y >>got-dashdash
y dup
s -nocache
y ==
y if
y >>set-nocache
y L0
y ref
y swap
y append
y L0
y set!
i 1
y +
y <<parse-cmdline
y @set-nocache
y drop
b true
y NOCACHE
y set!
i 1
y +
y <<parse-cmdline
y @got-dashdash
y drop
y cmdline-args
y swap
i -1
y slice
y L0
y ref
y swap
y +
y L0
y set!
y >>end
y @done
y drop
y @end
y L>
y SCRIPT_ARGS
y set!
s ../lib/repl.verb
y load-file-maybe-cached
y safe-run-and-delete-main
y repl-main
y return
