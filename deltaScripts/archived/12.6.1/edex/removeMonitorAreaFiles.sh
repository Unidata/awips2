#!/bin/bash
# This script will remove the existing monitoringArea files from
# fog, safeseas, and snow site directories.
#
# This update needs to be performed with build 12.6.1.
#

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

if [ -d /awips2/edex/data/utility/common_static/site ]; then
   cd /awips2/edex/data/utility/common_static/site
   if [ $? -ne 0 ]; then
      echo "FATAL: Site directories do not exist!"
      exit 1
   fi
   for site in `ls -1d *`;
   do
      for comp in fog safeseas snow
      do
         echo "INFO: Removing files from /awips2/edex/data/utility/common_static/site/$site/$comp/monitoringArea"
         if [ -d /awips2/edex/data/utility/common_static/site/${site}/${comp}/monitoringArea ]; then
            cd /awips2/edex/data/utility/common_static/site/${site}/${comp}/monitoringArea
            if [ $? -ne 0 ]; then
               echo "FATAL: Could not change directory to site/$site/$comp/monitoringArea directory"
            else
               rm /awips2/edex/data/utility/common_static/site/${site}/${comp}/monitoringArea/*
               if [ $? -ne 0 ]; then
                  echo "FATAL: Could not remove files from /awips2/edex/data/utility/common_static/site/$site/$comp/monitoringArea"
               fi
             fi
         fi
      done
   done
   echo "INFO: This update was successful."
fi

exit 0
