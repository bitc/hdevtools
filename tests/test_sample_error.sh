#!/bin/sh

set -e

SOCK=`mktemp -u`

EXPECTED_ERRORS='SampleError.hs:9:5: Not in scope: `foo'\'''

if ERRORS=`$HDEVTOOLS check --socket=$SOCK SampleError.hs`
then
	false
elsh
	[ "$ERRORS" = "$EXPECTED_ERRORS" ]
fi

$HDEVTOOLS --socket=$SOCK --stop-server
