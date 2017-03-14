#!/bin/bash
# DR #2061 - this update script will drop the dataURI column from acars

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

# takes three args: table, constraint name, unique columns
# will first drop the constraint if it exists and then adds it back, this is
# fairly inefficient if it does exist but operationally it won't exist and for
# testing this allows the script to be run easily as a noop.
function dropDatauriAndAddConstraint {
   dropDatauri $1
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP CONSTRAINT IF EXISTS $2;"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 ADD CONSTRAINT $2 UNIQUE $3;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to add new unique constraint for $table"
      echo "FATAL: The update has failed."
      exit 1
   fi
}

echo "INFO: Dropping acars dataURI columns."

dropDatauriAndAddConstraint acars acars_reftime_tailnumber_flightlevel_latitude_longitude_key "(reftime, tailnumber, flightlevel, latitude, longitude)"
${PSQL} -U awips -d metadata -c "update acars set stationid = to_char(longitude, 'FM999.999') || ':' || to_char(latitude, 'FM999.999')"
${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE acars"

echo "INFO: acars dataURI columns dropped successfully"
