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


# takes one arg: name of the index
function dropIndex {
   ${PSQL} -U awips -d metadata -c "DROP INDEX IF EXISTS \"$1\";"
}

# takes three args: table, index name, columns
# will first drop the index if it exists and then adds it back, this is
# fairly inefficient if it does exist but operationally it won't exist and for
# testing this allows the script to be run easily as a noop.
function dropAndAddIndex {
   ${PSQL} -U awips -d metadata -c "DROP INDEX IF EXISTS \"$2\";"
   ${PSQL} -U awips -d metadata -c "CREATE INDEX $2 ON $1 USING btree $3;"
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
}

# takes one arg: name of the table
function vacuumTable {
   ${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE $1"
}

echo "INFO: Dropping dataURI columns."

dropAndAddConstraint grid grid_reftime_forecasttime_info_id_rangestart_rangeend_key "(refTime, forecastTime, info_id, rangestart, rangeend)"
dropAndAddConstraint grid_info grid_info_datasetid_parameter_abbreviation_level_id_seconda_key "(datasetid, parameter_abbreviation, level_id, secondaryid, ensembleid, location_id)"
dropIndex gridDatasetReftime_idx
dropIndex grid_reftimeindex
dropIndex gridinfoNameParamLevel_idx
dropAndAddIndex grid grid_info_id_index "(info_id)"
dropDatauri
vacuumTable grid
vacuumTable grid_info


echo "INFO: grid dataURI column dropped successfully"
