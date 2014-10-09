#!/bin/bash
# DR #3644 - this update script will drop the dataURI column from ccfp

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

echo "INFO: Dropping ccfp dataURI columns."

dropDatauriAndAddConstraint ccfp ccfp_reftime_producttype_boxlat_boxlong_key "(reftime, producttype, boxlat, boxlong)"
${PSQL} -U awips -d metadata -c "DROP INDEX ccfp_reftimeindex;"
${PSQL} -U awips -d metadata -c "CREATE INDEX ccfp_reftimeindex ON ccfp USING btree (reftime);"
${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE ccfp"

echo "INFO: ccfp dataURI columns dropped successfully"
