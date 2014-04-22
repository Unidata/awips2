#!/bin/bash

echo "INFO: update started - removing collaboration local groups localization files"

find /awips2/edex/data/utility -type f -regex '.*collaboration/localGroups.xml$' -exec rm -vf {} \;

echo "INFO: the update has completed successfully!"

exit 0
