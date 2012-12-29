#!/bin/sh

set -e

SOCK=`mktemp -u`

$HDEVTOOLS check --socket=$SOCK Child.hs

PARENT=`$HDEVTOOLS modulefile --socket=$SOCK Parent`

[ "$PARENT" = "./Parent.hs" ]

$HDEVTOOLS --socket=$SOCK --stop-server
