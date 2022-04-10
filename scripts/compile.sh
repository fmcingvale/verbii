#!/bin/bash

REPL=`dirname $0`/../c++/repl
COMPILER=`dirname $0`/../verb/compile.verb

$REPL $COMPILER -- $@


