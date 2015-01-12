#!/bin/bash

# This script will rename the lightning source column in any D2D bundle/procedure files
# This update is only for edex servers which host the cave localization files

function usage()
{
    echo "Usage: $0 [-p|-b]"
    echo "Use '$0 -p to update procedure files"
    echo "Use '$0 -b to update bundle files"
}

if [[ $# < 1 ]]
then
    usage
    exit 1
fi

IFS=$'\n'
if [[ $1 == '-b' ]]
then
    files=`find /awips2/edex/data/utility/cave_static/*/*/bundles/ -iname '*.xml'`   
elif [[ $1 == '-p' ]]
then
    files=`ls /awips2/edex/data/utility/cave_static/*/*/procedures/*.xml`
else
    usage
    exit 1
fi

if [ $? -ne 0 ]; then
    echo "No files found."
    exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

xpath=".//resourceData[@{http://www.w3.org/2001/XMLSchema-instance}type='lightningResourceData']//mapping[@key='lightSource']"
attributeName="key"
replValue="source"

MY_DIR=`dirname $0`
for f in $files; do
    python $MY_DIR/replaceAttributeInXML.py $f $f.tmp $xpath $attributeName $replValue
    if [[ $? == 0 ]]
    then
        # if output file doesn't exist, xpath wasn't found
        if [[ -e $f.tmp ]]
        then
            mv $f.tmp $f
        fi
    else
        echo "ERROR: Problem updating file $f"
    fi
done



echo "INFO: The update finished successfully."
exit 0

