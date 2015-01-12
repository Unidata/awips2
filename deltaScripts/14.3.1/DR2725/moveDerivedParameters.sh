#!/bin/bash
# This script will move any non-base derived parameter functions and definitions
# from cave_static to common_static.
#
# This update is required with 14.3.1.
#

echo "INFO: Moving all derived parameter definitions and functions to common_static."

IFS=$'\n'
#                                                     LEVEL NAME
definitionFiles=`find /awips2/edex/data/utility/cave_static/*/*/derivedParameters/definitions/ -maxdepth 1 -iname '*.xml'`
functionFiles=`find /awips2/edex/data/utility/cave_static/*/*/derivedParameters/functions/ -maxdepth 1 -iname '*.py'`

for f in $definitionFiles; do
    newf=${f//cave_static/common_static}
    if [ -e "$newf" ]; then
        echo cannot upgrade $f because $newf already exists
    else
    	mkdir -p `dirname $newf`
        #echo "moving $f"
        mv "$f" "$newf"
    fi
done

for f in $functionFiles; do
    newf=${f//cave_static/common_static}
    if [ -e "$newf" ]; then
        echo cannot upgrade $f because $newf already exists
    else
        mkdir -p `dirname $newf`
        #echo "moving $f"
        mv "$f" "$newf"
    fi
done

echo "INFO: The update finished successfully."
exit 0
