#!/bin/bash

# Omaha #3690: Finds empty localization directories and deletes them with the 
# '-d' flag. This can improve performance in some areas of the system.

if [[ $1 == '-d' ]]
then
    echo "Delete all directory trees that don't contain regular files? [y/n]"
    read answer
    if [[ $answer == 'y' || $answer == 'Y' ]]
    then
        DEL_OPTS='-delete -print'
        echo "Deleting the following directories"
    else
        echo "Aborting"
        exit 1
    fi
fi

find /awips2/edex/data/utility -type d -empty $DEL_OPTS
