#!/bin/bash
# This script will add an index to sfcobs
#
# This needs to be performed with build 11.9.0-3.
#

PSQL="/awips2/psql/bin/psql"
SQL_COMMAND="CREATE INDEX reporttype_index ON sfcobs USING btree (reporttype);"

if [ ! -f ${PSQL} ]; then
echo "ERROR: The PSQL executable does not exist - ${PSQL}."
echo "FATAL: Update Failed!"
exit 1
fi

echo ""
echo "Press Enter to perform the updates Ctrl-C to quit."
read done

${PSQL} -U awips -d metadata -c "${SQL_COMMAND}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

echo "INFO: The update was successfully applied."

exit 0
