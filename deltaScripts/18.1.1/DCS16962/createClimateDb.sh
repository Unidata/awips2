#!/bin/bash

SQL_SCRIPT="createClimateDb.sql"
TABLESPACE_DIR="/awips2/database/tablespaces/climate"

# ensure that the sql script is present
if [ ! -f ${SQL_SCRIPT} ]; then
   echo "ERROR: the required sql script - ${SQL_SCRIPT} was not found."
   echo "FATAL: the update has failed!"
   exit 1
fi

echo "INFO: update started - creating Climate tablespace directory"

# ensure tablespace directory created
mkdir -p ${TABLESPACE_DIR}
if [ ! -d ${TABLESPACE_DIR} ]; then
   echo "ERROR: the required directory - ${TABLESPACE_DIR} was not created."
   echo "FATAL: the update has failed!"
   exit 1
fi

echo "INFO: creating Climate DB"

# run the update
/awips2/psql/bin/psql -U awipsadmin -d metadata -f ${SQL_SCRIPT}
if [ $? -ne 0 ]; then
   echo "FATAL: the update has failed!"
   exit 1
fi

echo "INFO: the update has completed successfully!"

exit 0
