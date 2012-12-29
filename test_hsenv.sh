#!/bin/bash

set -e

if [ $# -lt 1 ]
then
	echo "Usage:"
	echo "$0 <hsenv_name> [<hsenv_name2> ...]"
	exit 2
fi

for i in $*
do
	source .hsenv_$i/bin/activate
	cabal build
	export HDEVTOOLS=./dist_$i/build/hdevtools/hdevtools
	./tests/test_runner.sh
	deactivate_hsenv
done

echo
echo 'All Tests Passed in:' $*
