#!/bin/bash
# This script will remove the gfe vcmodule directory.
#
# This update needs to be performed with build 11.9.0-6.
#

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

if [ -d /awips2/edex/data/utility/common_static/base/gfe/vcmodule ]; then
   echo "INFO: removing /awips2/edex/data/utility/common_static/base/gfe/vcmodule."

   rm -rfv /awips2/edex/data/utility/common_static/base/gfe/vcmodule/*
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to remove the contents of /awips2/edex/data/utility/common_static/base/gfe/vcmodule."
      echo "FATAL: Updated failed!"
      exit 1
   fi
else
   echo "INFO: No updates to perform."
fi

exit 0
