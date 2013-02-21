#!/bin/bash
# This script will add register the gridcoverage plugin, which was previously part of grib
#
# This needs to be performed with build ????
#

PSQL="/awips2/psql/bin/psql"
SQL_COMMAND_CHECK="select * FROM gridcoverage LIMIT 1;"
SQL_COMMAND_REGISTER="delete from plugin_info where name = 'gridcoverage';"
SQL_COMMAND_SEQ="DROP SEQUENCE gridcoverage_seq;"
SQL_COMMAND_UPDATE_ID="update gridcoverage set id=id*10000;"
SQL_COMMAND_ALTER_NAME_DESC="ALTER TABLE gridcoverage ADD COLUMN description character varying(3071), ALTER COLUMN name TYPE character varying(2047);"

if [ ! -f ${PSQL} ]; then
echo "ERROR: The PSQL executable does not exist - ${PSQL}."
echo "FATAL: Update Failed!"
exit 1
fi

echo ""
echo "Press Enter to undo the updates Ctrl-C to quit."
read done

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_CHECK}" > /dev/null
if [ $? -ne 0 ]; then
echo "WARN: gridcoverage table does not exist so we are not registering the plugin"
exit 0
fi

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_REGISTER}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_SEQ}"
if [ $? -ne 0 ]; then
echo "FATAL: unable to delete gridcoverage_seq"
exit 1
fi

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_UPDATE_ID}"
if [ $? -ne 0 ]; then
echo "FATAL: unable to update gridcoverage ids"
exit 1
fi

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_ALTER_NAME_DESC}"
if [ $? -ne 0 ]; then
echo "WARN: unable to add description column to gridcoverage table"
fi

echo "INFO: The update was successfully removed."
exit 0
