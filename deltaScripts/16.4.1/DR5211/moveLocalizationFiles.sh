#!/bin/bash
# This script will move any non-base localization files to common_static.
#
# This update is required with 16.4.1.
#
# This update is only for edex servers which host the localization files
#

echo "INFO: Moving localization files to common_static."

IFS=$'\n'

function moveFiles {
    type=$1
    dir=$2
    echo "INFO: Moving $dir from $type to common_static."
    commonFiles=`find /awips2/edex/data/utility/$type/*/*/$dir/ -type f`
    for f in $commonFiles; do
        newf=${f//$type/common_static}
        if [[ $f == *.md5 ]]; then
            rm $f
        elif [ -e "$newf" ]; then
            echo Cannot upgrade $f because $newf already exists
        else
            mkdir -p `dirname $newf`
            mv "$f" "$newf"
        fi
    done
    find /awips2/edex/data/utility/$type/*/*/$dir/ -type d -empty -delete
    find /awips2/edex/data/utility/$type/base/$dir/ -iname "*.md5" -size 32c -delete
    find /awips2/edex/data/utility/$type/base/$dir/ -type d -empty -delete    
}

moveFiles edex_static grib
moveFiles edex_static grid
moveFiles edex_static distribution

echo "INFO: The update finished successfully."
exit 0
