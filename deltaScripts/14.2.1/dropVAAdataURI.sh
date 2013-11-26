#!/bin/bash
# DR #2582 - this update script will drop the dataURI column from the vaa table

PSQL="/awips2/psql/bin/psql"

# takes one arg: a table name
# drops the datauri constraint and column if they exist
function dropDatauri {
   echo "INFO: Dropping DataURI column from $1"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP CONSTRAINT IF EXISTS ${1}_datauri_key;"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP COLUMN IF EXISTS datauri;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to drop dataURI column for $table"
      echo "FATAL: The update has failed."
      exit 1
   fi
}


function dropRecordType {
   echo "INFO: Dropping recordType column from $1"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP COLUMN IF EXISTS recordtype;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to drop recordType column for $table"
      echo "FATAL: The update has failed."
      exit 1
   fi
}


# takes three args: table, constraint name, unique columns
# will first drop the constraint if it exists and then adds it back, this is
# fairly inefficient if it does exist but operationally it won't exist and for
# testing this allows the script to be run easily as a noop.
function dropDatauriAndAddConstraint {
   dropDatauri $1
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP CONSTRAINT IF EXISTS $2;"
   dropRecordType $1
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 ADD CONSTRAINT $2 UNIQUE $3;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to add new unique constraint for $table"
      echo "FATAL: The update has failed."
      exit 1
   fi
   ${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE $1"
}

echo "INFO: Dropping dataURI columns."

dropDatauriAndAddConstraint vaa vaa_latitude_longitude_stationId_reftime_forecasttime_advisoryNumber_key "(latitude, longitude, stationId, reftime, forecasttime, advisoryNumber)"


echo "INFO: VAA dataURI column dropped successfully"
