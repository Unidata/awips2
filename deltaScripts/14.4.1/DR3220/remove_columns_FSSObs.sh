#!/bin/bash
# DR 3220 - Remove metadata columns from FSSObs database

PSQL="/awips2/psql/bin/psql"

echo "INFO: Removing columns metadata cwa and monitoruse from table ffsobs"

${PSQL} -U awips -d metadata -c "ALTER TABLE fssobs DROP COLUMN cwa, DROP COLUMN monitoruse;"

if [ $? -ne 0 ]; then
   echo "ERROR: Failed to remove columns metadata from table fssobs"
   echo "FATAL: The update has failed."
   exit 1
fi

echo "INFO: column metadata removed successfully"
