L 13
W make-var
L 22
o 65537
o 1
i 1
y alloc
o 131073
o 65536
o 131072
y set!
o 131072
y 'ref
i 2
y make-list
o 0
y make-word
o 131072
y 'set!
i 2
y make-list
o 0
y '!
y +
y make-word
W make-const
L 14
o 65537
o 1
i 1
y alloc
o 131073
o 65536
o 131072
y set!
o 131072
y 'ref
i 2
y make-list
o 0
y make-word
W safe-run-and-delete-main
L 6
y '__main__
y .dumpword
y '__main__
y ,,del
y make-lambda
y call
W deserialize-and-run
L 4
o 1
o 0
y deserialize
y safe-run-and-delete-main
W find-filename-module-path
L 37
o 1
i 0
o 131073
y @loop
y IMPORT-SEARCH-PATHS
o 131072
y get
y void?
y if
y >>not-found
y IMPORT-SEARCH-PATHS
o 131072
y get
o 0
y +
o 65537
o 65536
y file-exists?
y if
y >>found
o 131072
i 1
y +
o 131073
y <<loop
y @not-found
s Unable%32to%32find%32module%32'
o 0
y +
s '%32on%32path%32
y +
y IMPORT-SEARCH-PATHS
y str
y +
y error
y @found
o 65536
W add-standard-import-search-paths
L 7
y IMPORT-SEARCH-PATHS
y os-getcwd
y file-pathsep
y +
y append
y IMPORT-SEARCH-PATHS!
y return
W init-interpreter
L 17
s init.verb.b
y find-filename-module-path
y deserialize-and-run
s compiler.verb.b
y find-filename-module-path
y deserialize-and-run
s patches.verb
y find-filename-module-path
y file-read
b true
y set-allow-overwrite-words
y compile-and-load-string
y __main__
y '__main__
y ,,del
b false
y set-allow-overwrite-words
W load-file-maybe-cached
L 11
o 1
y *_NOCACHE
y if
y >>nocache
o 0
y cached-compile-and-load
y return
y @nocache
o 0
y file-read
y compile-and-load-string
W import
L 29
y *_IMPORTED-MODULES
y over
y get
y void?
y not
y if
y >>already-imported
y dup
y *_IMPORTED-MODULES
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
W pathname-ends-in-separator?
L 20
o 1
o 0
i -1
y get
s \
y ==
y if
y >>yes
o 0
i -1
y get
s /
y ==
y if
y >>yes
b false
y return
y @yes
b true
y return
W .dbg
L 9
o 1
o 0
i 13
y chr
y +
i 10
y chr
y +
y puts-stderr
W boot-main
L 113
o 1
y add-standard-import-search-paths
L 0
y SCRIPT-ARGS!
i 0
o 65537
y @parse-cmdline
o 0
o 65536
y get
y void?
y if
y >>done
o 0
o 65536
y get
s --
y ==
y if
y >>got-dashdash
o 0
o 65536
y get
s -nocache
y ==
y if
y >>set-nocache
o 0
o 65536
y get
s -libdir
y ==
y if
y >>got-libdir
y SCRIPT-ARGS
o 0
o 65536
y get
y append
y SCRIPT-ARGS!
o 65536
i 1
y +
o 65537
y <<parse-cmdline
y @set-nocache
b true
y *_NOCACHE!
o 65536
i 1
y +
o 65537
y <<parse-cmdline
y @got-dashdash
y SCRIPT-ARGS
o 0
o 65536
i -1
y slice
y +
y SCRIPT-ARGS!
y >>done
y @got-libdir
o 0
o 65536
i 1
y +
y get
y void?
y if
y >>missing-libdir
o 0
o 65536
i 1
y +
y get
y pathname-ends-in-separator?
y if
y >>good-libdir
y >>missing-libdir-sep
y @good-libdir
y IMPORT-SEARCH-PATHS
o 0
o 65536
i 1
y +
y get
y append
y IMPORT-SEARCH-PATHS!
o 65536
i 2
y +
o 65537
y <<parse-cmdline
y @missing-libdir
s Path%32is%32required%32after%32-libdir
y error
y @missing-libdir-sep
s Paths%32passed%32to%32-libdir%32must%32end%32in%32\%32or%32/,%32got:%32
o 0
o 65536
i 1
y +
y get
y +
y error
y @done
y init-interpreter
s repl.verb
y find-filename-module-path
y load-file-maybe-cached
y safe-run-and-delete-main
y repl-main
W __main__
L 13
y 'IMPORT-SEARCH-PATHS
L 0
y make-var
y '*_NOCACHE
b false
y make-var
y '*_IMPORTED-MODULES
y ,,new-dict
y make-var
y 'SCRIPT-ARGS
L 0
y make-var
y boot-main
