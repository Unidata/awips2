#!/bin/bash

# DR 5348 - This script will replace references to us_county.lpi in any bundles or procedure with
# references to the new county_names view in the maps database

IFS=$'\n'
files=`find /awips2/edex/data/utility/cave_static/*/*/bundles/ /awips2/edex/data/utility/cave_static/*/*/procedures/ -iname '*.xml'`   

MY_DIR=`dirname $0`
for f in $files; do
    python $MY_DIR/replaceCountyNamesLpi.py $f $f.tmp
    if [[ $? == 0 ]]
    then
        # if output file doesn't exist, no changes were made
        if [[ -e $f.tmp ]]
        then
            mv $f.tmp $f
            echo "Updated: $f"
        fi
    else
        echo "ERROR: Problem updating file $f"
    fi
done



echo "INFO: The update finished successfully."
exit 0

