#!/bin/bash

# Omaha #3690
# Find and print empty localization directories. If the '-d' option is specified, then these
# directories will be deleted. This can improve performance in some areas of the system.

# Only run this script on a single EDEX server (dv3 or dv4). Run only when all EDEX are down.

if [[ $1 == '-d' ]]
then
    DEL_OPTS='-delete -print'
    echo "Deleting the following directories"
fi

find /awips2/edex/data/utility -xdev -type d -empty $DEL_OPTS
