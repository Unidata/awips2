#!/bin/bash
# DR #1869 - this update script will drop the dataURI column from all tables
# where it is no longer needed.

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
   ${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE $1"
}

echo "INFO: Dropping dataURI columns."

# GFE already has constraints right so just drop the column and vaccuum
dropDatauri gfe
${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE gfe"

# Remap the constraints for these type
dropDatauriAndAddConstraint bufrmosavn bufrmosavn_location_id_reftime_forecasttime_key "(location_id, reftime, forecasttime)"
dropDatauriAndAddConstraint bufrmoshpc bufrmoshpc_location_id_reftime_forecasttime_key "(location_id, reftime, forecasttime)"
dropDatauriAndAddConstraint goessounding goessounding_stationid_reftime_latitude_longitude_key "(stationid, reftime, latitude, longitude)"
dropDatauriAndAddConstraint poessounding poessounding_stationid_reftime_latitude_longitude_key "(stationid, reftime, latitude, longitude)"
dropDatauriAndAddConstraint ldadmesonet ldadmesonet_stationid_reftime_reportType_dataProvider_latitude_longitude_key "(stationid, reftime, reportType, dataProvider, latitude, longitude)"
dropDatauriAndAddConstraint qc qc_stationid_reftime_qctype_latitude_longitude_key "(stationid, reftime, qcType, latitude, longitude)"

# These type need a unique stationid so set one before dropping datauri.
${PSQL} -U awips -d metadata -c "update bufrascat set stationid = to_char(longitude, 'FM999.999') || ':' || to_char(latitude, 'FM999.999')"
dropDatauriAndAddConstraint bufrascat bufrascat_stationid_reftime_satid_latitude_longitude_key "(stationid, reftime, satid, latitude, longitude)"
${PSQL} -U awips -d metadata -c "update bufrssmi set stationid = to_char(longitude, 'FM999.999') || ':' || to_char(latitude, 'FM999.999')"
dropDatauriAndAddConstraint bufrssmi bufrssmi_stationid_reftime_satid_latitude_longitude_key "(stationid, reftime, satid, latitude, longitude)"
${PSQL} -U awips -d metadata -c "update bufrhdw set stationid = to_char(longitude, 'FM999.999') || ':' || to_char(latitude, 'FM999.999')"
dropDatauriAndAddConstraint bufrhdw bufrhdw_stationid_reftime_sattype_pressure_latitude_longitude_key "(stationid, reftime, sattype, pressure, latitude, longitude)"
${PSQL} -U awips -d metadata -c "update bufrmthdw set stationid = to_char(longitude, 'FM999.999') || ':' || to_char(latitude, 'FM999.999')"
dropDatauriAndAddConstraint bufrmthdw bufrmthdw_stationid_reftime_sattype_pressure_latitude_longitude_key "(stationid, reftime, sattype, pressure, latitude, longitude)"


echo "INFO: dataURI columns dropped successfully"
