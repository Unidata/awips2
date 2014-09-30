#!/bin/bash
# This script will update any D2D procedures files
# which use the old Topo file name

IFS=$'\n'
files=`ls /awips2/edex/data/utility/cave_static/*/*/procedures/*.xml`

if [ $? -ne 0 ]; then
echo "No procedures found"
exit 1
fi

MY_DIR=`dirname $0`

for f in $files; do
    grep 'srtm30.hdf' $f > /dev/null
    if [ $? -eq 0 ]; then
        echo Updating $f
        python $MY_DIR/updateTopoFile.py $f
    fi
done

echo "INFO: the update has completed successfully!"
exit 0
