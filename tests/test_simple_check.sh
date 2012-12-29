#!/bin/sh

set -e

SOCK=`mktemp -u`

ERRORS=`$HDEVTOOLS check --socket=$SOCK Simple.hs`

[ -z "$ERRORS" ]

$HDEVTOOLS --socket=$SOCK --stop-server
