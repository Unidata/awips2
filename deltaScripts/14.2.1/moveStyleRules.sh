#!/bin/bash
# This script will move any non-base style rules from cave_static to common_static.
#
# This update is required with 14.2.1.
#
# This update is only for edex servers which host the cave localization files
#

echo "INFO: Moving all style rules to common_static."

IFS=$'\n'
commonFiles=`find /awips2/edex/data/utility/cave_static/*/*/styleRules/ -iname '*.xml'`

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
