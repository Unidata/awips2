#!/bin/bash
# This script will add create tables for the grid plugin
#
# This needs to be performed with build ????
#

PSQL="/awips2/psql/bin/psql"
GRID_COMMAND="DROP TABLE grid;"
INFO_SEQ_COMMAND="DROP SEQUENCE gridinfo_seq;"
INFO_COMMAND="DROP TABLE grid_info;"
PARAM_COMMAND="DROP TABLE parameter;"
SQL_COMMAND_REGISTER="delete from plugin_info where name = 'grid' OR name = 'parameter';"


if [ ! -f ${PSQL} ]; then
echo "ERROR: The PSQL executable does not exist - ${PSQL}."
echo "FATAL: Update Failed!"
exit 1
fi

echo ""
echo "Press Enter to undo the updates Ctrl-C to quit."
read done

${PSQL} -U awips -d metadata -c "${SQL_COMMAND_REGISTER}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

${PSQL} -U awips -d metadata -c "${GRID_COMMAND}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

${PSQL} -U awips -d metadata -c "${INFO_COMMAND}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

${PSQL} -U awips -d metadata -c "${INFO_SEQ_COMMAND}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi

${PSQL} -U awips -d metadata -c "${PARAM_COMMAND}"
if [ $? -ne 0 ]; then
echo "FATAL: Update Failed!"
exit 1
fi


echo "INFO: The update was successfully applied."

exit 0