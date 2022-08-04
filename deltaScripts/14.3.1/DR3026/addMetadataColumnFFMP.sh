#!/bin/bash
# DR 3026 - Add metadata column to FFMP database

PSQL="/awips2/psql/bin/psql"

echo "INFO: Adding column metadata to table ffmp"

${PSQL} -U awips -d metadata -c "ALTER TABLE ffmp ADD COLUMN metadata varchar(255);"

if [ $? -ne 0 ]; then
   echo "ERROR: Failed to add column metadata to table ffmp"
   echo "FATAL: The update has failed."
   exit 1
fi

echo "INFO: column metadata added successfully"
