#!/bin/sh

set -e

SOCK=`mktemp -u`

echo '> Starting the server'
$HDEVTOOLS --socket=$SOCK --start-server

echo '> Checking status'
$HDEVTOOLS --socket=$SOCK --status

echo '> Checking that the socket file exists'
if [ ! -S $SOCK ]; then false; fi

echo '> Stopping the server'
$HDEVTOOLS --socket=$SOCK --stop-server

echo '> Checking that the socket file no longer exists'
if [ -e $SOCK ]; then false; fi
