#! /bin/bash
# DR #4911 - This script drops unused columns from the common_obs_spatial table

PSQL="/awips2/psql/bin/psql"

columns=( "aerodromeflag" "rbsnindicator" "pressurelevel" )

echo "INFO: Updating common_obs_spatial"

for column in ${columns[@]} ; do 
      ${PSQL} -U awips -d metadata -c "ALTER TABLE IF EXISTS common_obs_spatial DROP COLUMN IF EXISTS ${column} ;"
done

