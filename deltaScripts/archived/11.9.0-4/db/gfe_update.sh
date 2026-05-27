#!/bin/bash
# This script will set the initialization flag for gfe to FALSE in plugin_info.
# The next time edex starts, an additional table will be created for gfe.
#
# This update needs to be performed with build 11.9.0-4.
#

PSQL="/awips2/psql/bin/psql"
SQL_COMMAND="UPDATE plugin_info SET initialized='false' WHERE name='com.raytheon.edex.plugin.gfe'"

if [ ! -f ${PSQL} ]; then
   echo "ERROR: The PSQL Executable does not exist - ${PSQL}."
   echo "FATAL: Updated Failed!"
   exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

echo "INFO: Updating plugin_info."
${PSQL} -U awips -d metadata -c "${SQL_COMMAND}"
if [ $? -ne 0 ]; then
   echo "FATAL: Updated Failed!"
   exit 1
fi

echo "INFO: The updated was successfully applied."

exit 0
