#!/bin/bash
# DR #2060 - this update script will drop the dataURI column from the grid table

PSQL="/awips2/psql/bin/psql"

# drops the datauri constraint and column if they exist
function dropDatauri {
   echo "INFO: Dropping DataURI column from grid"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE grid DROP CONSTRAINT IF EXISTS grid_datauri_key;"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE grid DROP COLUMN IF EXISTS datauri;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to drop dataURI column for grid"
      echo "FATAL: The update has failed."
      exit 1
   fi
}


# takes three args: table, constraint name, unique columns
# will first drop the constraint if it exists and then adds it back, this is
# fairly inefficient if it does exist but operationally it won't exist and for
# testing this allows the script to be run easily as a noop.
function dropAndAddConstraint {
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 DROP CONSTRAINT IF EXISTS $2;"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 ADD CONSTRAINT $2 UNIQUE $3;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to add new unique constraint for $1"
      echo "FATAL: The update has failed."
      exit 1
   fi
   ${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE $1"
}

echo "INFO: Dropping dataURI columns."

dropAndAddConstraint grid grid_reftime_forecasttime_rangestart_rangeend_info_id "(refTime, forecastTime, rangestart, rangeend, info_id)"
dropAndAddConstraint grid_info grid_info_datasetid_secondaryid_ensembleid_location_id_parameter_abbreviation_level_id "(datasetid, secondaryid, ensembleid, location_id, parameter_abbreviation, level_id)"
dropDatauri


echo "INFO: grid dataURI column dropped successfully"
