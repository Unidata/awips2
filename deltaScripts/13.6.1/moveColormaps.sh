#!/bin/bash
# This script will move any non-base colormaps from cave_static to common_static.
#
# This update is required with 13.6.1.
#
# This update is only for edex servers which host the cave localization files
#

echo "INFO: Moving all colormaps to common_static."

IFS=$'\n'
commonFiles=`find /awips2/edex/data/utility/cave_static/*/*/colormaps/ -iname '*.cmap'`

for f in $commonFiles; do
    newf=${f//cave_static/common_static}
    if [ -e "$newf" ]; then
        echo Cannot upgrade $f because $newf already exists
    else
    	mkdir -p `dirname $newf`
        mv "$f" "$newf"
    fi
done

echo "INFO: The update finished successfully."
exit 0
