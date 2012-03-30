#!/bin/sh
$1 = .
# if [ $# -ne 1 ]; then echo "Usage: $0 [dir]"; exit 1; fi

FILES=`find $1`

for file in $FILES
do
		echo Checking in $file...
		cvs add $file
		if [ -f $file ]
		then
				cvs commit -m "add $file" $file
		fi
done
