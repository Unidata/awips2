#!/bin/bash
# This script will drop the fog table and remove the fog hdf5 files.
#
# This update needs to be performed with build 11.9.0-3.
#

PSQL="/awips2/psql/bin/psql"
SQL_COMMAND="DROP TABLE IF EXISTS fog; UPDATE plugin_info SET initialized='false' WHERE name='fog';"

if [ ! -f ${PSQL} ]; then
   echo "ERROR: The PSQL executable does not exist - ${PSQL}."
   echo "FATAL: Updated Failed!"
   exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

echo "INFO: Dropping the metadata fog table."
${PSQL} -U awips -d metadata -c "${SQL_COMMAND}"
if [ $? -ne 0 ]; then
   echo "FATAL: Updated Failed!"
   exit 1
fi

echo "INFO: Purging fog hdf5 files."
if [ -d /awips2/edex/data/hdf5/fog ]; then
   rm -rfv /awips2/edex/data/hdf5/fog
fi

echo "INFO: The update was successfully applied."

exit 0
