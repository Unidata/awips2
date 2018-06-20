#!/bin/bash
# This script will drop the ntrans table and remove the ntrans hdf5 files.
#
# This update needs to be performed when going from build 13.3.1 to build 13.4.1 (or 13.5.1).
#

PSQL="/awips2/psql/bin/psql"
SQL_COMMAND="DROP TABLE IF EXISTS ntrans; UPDATE plugin_info SET initialized='false' WHERE name='ntrans';"

if [ ! -f ${PSQL} ]; then
   echo "ERROR: The PSQL executable does not exist - ${PSQL}."
   echo "FATAL: Updated Failed!"
   exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

echo "INFO: Dropping the metadata ntrans table."
${PSQL} -U awips -d metadata -c "${SQL_COMMAND}"
if [ $? -ne 0 ]; then
   echo "FATAL: Updated Failed!"
   exit 1
fi

echo "INFO: Purging ntrans hdf5 files."
if [ -d /awips2/edex/data/hdf5/ntrans ]; then
   rm -rfv /awips2/edex/data/hdf5/ntrans
fi

echo "INFO: The update was successfully applied."

exit 0
