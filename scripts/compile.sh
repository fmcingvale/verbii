#!/bin/bash

VERBII=`dirname $0`/../cpp/verbii
COMPILER=`dirname $0`/../verb/compile.verb

$VERBII $COMPILER -- $@


