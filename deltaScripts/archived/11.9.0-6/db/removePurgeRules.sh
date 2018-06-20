#!/bin/bash
# This script will drop the purge table in the metadata database.
#
# This update needs to be performed with build 11.9.0-6.
#

PSQL="/awips2/psql/bin/psql"
SQL_COMMAND="DROP TABLE awips.purge_rules;"

if [ ! -f ${PSQL} ]; then
   echo "ERROR: The psql executable does not exist."
   echo "FATAL: Updated failed!"
   exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

echo "INFO: Dropping the awips.purge_rules table."
${PSQL} -U awips -d metadata -c "${SQL_COMMAND}"
if [ $? -ne 0 ]; then
   echo "FATAL: Updated failed!"
   exit 1
fi

echo "INFO: The update was successfully applied."

exit 0
