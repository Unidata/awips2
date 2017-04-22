#!/bin/bash
# DR #4209 - this update script will convert the satellite_spatial dx and dy columns to double precision
# This is only necessary on sites installed before 14.2 but there is no harm running it everywhere.

PSQL="/awips2/psql/bin/psql"

echo "INFO: Updating satellite spatial table"

${PSQL} -U awips -d metadata -c "ALTER TABLE satellite_spatial ALTER COLUMN dx SET DATA TYPE double precision;"
if [ $? -ne 0 ]; then
 echo "ERROR: Failed to update column type for dx column of satellite_spatial table"
 echo "FATAL: The update has failed."
 exit 1
fi
${PSQL} -U awips -d metadata -c "ALTER TABLE satellite_spatial ALTER COLUMN dy SET DATA TYPE double precision;"
if [ $? -ne 0 ]; then
 echo "ERROR: Failed to update column type for dy column of satellite_spatial table"
 echo "FATAL: The update has failed."
 exit 1
fi

echo "INFO: Satellite spatial table successfully updated."
