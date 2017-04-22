#!/bin/bash
# This script will remove any existing log files from
# /awips2/GFESuite/logs. 
#
# This update needs to be performed with build 11.9.0-7.
# This update is for both the database and the processing
# servers.

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

if [ -d /awips2/GFESuite/logs ]; then
   echo "INFO: Removing GFESuite Logs."
   rm -rf /awips2/GFESuite/logs/*
   if [ $? -ne 0 ]; then
      echo "ERROR: Unable to remove the contents of /awips2/GFESuite/logs."
      echo "FATAL: Update Failed!"
      exit 1
   fi
fi

echo "INFO: The update finished successfully."
exit 0
