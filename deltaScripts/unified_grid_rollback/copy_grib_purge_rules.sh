#!/bin/bash
# This script will copy any grib purge rules to a equivalent grid purge rules file
#
# This update needs to be performed with build ???.

echo ""
echo "Press Enter to undo the updates Ctrl-C to quit."
read done

IFS=$'\n'
files=`find /awips2/edex/data/utility/common_static/site/*/purge/gridPurgeRules.xml`

if [ $? -ne 0 ]; then
echo "No site level grid purge files found!"
exit 0
fi

for f in $files; do
    echo Deleting $f
    rm $f
done

echo "INFO: The update was successfully removed."
exit 0
