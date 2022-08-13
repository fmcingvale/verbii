#!/bin/bash

REPL=`dirname $0`/../cpp/repl
COMPILER=`dirname $0`/../verb/compile.verb

$REPL $COMPILER -- $@


