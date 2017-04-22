#!/bin/bash
# DR5527 Removing CASA nswrc-radial data from database

PSQL_COMMAND="/awips2/psql/bin/psql"

if [ ! -f ${PSQL_COMMAND} ]; then
 echo "ERROR: The POSTGRES_DB executable does not exist - ${PSQL_COMMAND}."
 echo "FATAL: Update Failed!"
 exit 1
fi

${PSQL_COMMAND} -U awips -d metadata -c "truncate table nswrcradial"

echo "Removal of radial-data from 'nswrcradial' table complete."
exit 0