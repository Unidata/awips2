#!/bin/bash
# DR #1869 - this update script will drop the dataURI column from all tables
# where it is no longer needed.

PSQL="/awips2/psql/bin/psql"

echo "INFO: Dropping dataURI columns."

# TODO this script will need to be extended for unique constraint.
for table in gfe;
do
   echo "INFO: Dropping DataURI column from $table"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $table DROP COLUMN IF EXISTS datauri;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to drop dataURI column for $table"
      echo "FATAL: The update has failed."
      exit 1
   fi
done