#!/bin/bash
#
# Concatenate grib and bufr files before ingesting them into edex
# to reduce the number of jobs edex processes
#
# Author: srcarter@ucar.edu
# Author: tiffanym@ucar.edu
#
# Updates
# Jan 12, 2024 - First stab at translating from our existing perl script
#
#

# We want to concatenate grib and bufr files
paths=("/awips2/data_store/grid" "/awips2/data_store/modelsounding")

for path in "${paths[@]}";
do
	# Check for relevant files that haven't been touched in the last two minutes
	IFS=$'\n'
	files=($(find $path -name "*-concat-*" -mmin +2 | grep "staging"))
	unset IFS

#	echo ${#files[@]}
	for file in "${files[@]}";
	do
		/awips2/python/bin/python /awips2/fxa/bin/src/qpidNotify/qpidNotify.py $file
		echo $file
	done
done

