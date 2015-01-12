#!/bin/bash
# DR3708 updates collaboration config to support blacklists

IFS=$'\n'
files=`find /awips2/edex/data/utility/cave_static/*/*/collaboration/ -name 'config.xml'`   

if [ $? -ne 0 ]; then
    echo "No files found."
    exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

MY_DIR=`dirname $0`
for f in $files; do
    echo "Updating $f"
    xml=$(python $MY_DIR/util/updateCollaborationConfig.py $f)
    if [[ $? != 0 ]]
    then
        echo "ERROR: Problem updating file $f"
    elif [[ -n $xml ]]
    then
        echo $xml | xmllint --format - > $f
        echo "Successfully updated"
    else
        echo "No update needed for $f"
    fi
done
