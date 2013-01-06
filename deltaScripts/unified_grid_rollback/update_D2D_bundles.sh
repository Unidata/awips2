#!/bin/bash
# This script will update any D2D bundle files
# to use grid data in place of grib
#
# This update needs to be performed with build ???.
# This update is only for edex servers which host the cave localization files


echo ""
echo "Press Enter to undo the updates Ctrl-C to quit."
read done

IFS=$'\n'
files=`find /awips2/edex/data/utility/cave_static/*/*/bundles/ -iname '*.xml'`

if [ $? -ne 0 ]; then
echo "No bundle files found."
exit 1
fi

MY_DIR=`dirname $0`

for f in $files; do
	bash $MY_DIR/update_saved_display.sh $f
done



echo "INFO: The update was successfully removed."
exit 0
