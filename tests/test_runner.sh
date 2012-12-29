#!/bin/sh

set -e

ALL_TESTS="\
	test_start_stop.sh \
	test_simple_check.sh \
	test_sample_error.sh \
	test_module_file.sh \
	"

if [ ! $HDEVTOOLS ]
then
	echo 'You must set the HDEVTOOLS environment variable to the path of the hdevtools binary'
	exit 1
fi

case "$HDEVTOOLS" in
	*/*)
		# Convert relative path to absolute:
		export HDEVTOOLS=`pwd`/$HDEVTOOLS
esac

echo $HDEVTOOLS

if [ $# -ne 0 ]
then
	TESTS=$*
else
	TESTS=$ALL_TESTS
	echo 'Running All Tests'
fi

echo '------------------------------------------------------------------------'

cd `dirname $0`

ERRORS=0
for i in $TESTS
do
	echo $i
	echo
	if sh $i
	then
		echo 'Test OK'
	else
		echo 'Test FAILED'
		ERRORS=`expr $ERRORS + 1`
	fi
	echo '------------------------------------------------------------------------'
done

if [ $ERRORS = 0 ]
then
	echo 'All Tests OK'
else
	echo $ERRORS 'FAILED Tests'
fi
exit $ERRORS
