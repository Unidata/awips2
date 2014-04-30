#!/bin/bash
# This script will remove the existing purge configuration files
# from base. The existing site-specifc purge configuration files
# will be copied from edex_static to common_static.
#
# This update needs to be performed with build 11.9.0-6.
#

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

echo "INFO: removing /awips2/edex/data/utility/edex_static/base/purge."

if [ -d /awips2/edex/data/utility/edex_static/base/purge ]; then
   rm -rfv /awips2/edex/data/utility/edex_static/base/purge
   if [ $? -ne 0 ]; then
      echo "FATAL: Updated failed!"
      exit 1
   fi
fi

echo "INFO: copying site-specifc purge rules to common_static."
if [ -d /awips2/edex/data/utility/edex_static/site ]; then
   cd /awips2/edex/data/utility/edex_static/site
   for site in `ls -1d *`;
   do
      if [ -d ${site}/purge ]; then
         if [ ! -d /awips2/edex/data/utility/common_static/site/${site} ]; then
            mkdir -p /awips2/edex/data/utility/common_static/site/${site}
            if [ $? -ne 0 ]; then
               echo "ERROR: Failed to create /awips2/edex/data/utility/common_static/site/${site}."
               echo "FATAL: Updated failed!"
               exit 1
            fi
         fi
         mv -v ${site}/purge /awips2/edex/data/utility/common_static/site/${site}
         if [ $? -ne 0 ]; then
            echo "ERROR: Failed to move ${site}/purge to /awips2/edex/data/utility/common_static/site/${site}."
            echo "FATAL: Updated failed!"
            exit 1
         fi
      fi
   done
fi

echo "INFO: The update was successfully applied."

exit 0
