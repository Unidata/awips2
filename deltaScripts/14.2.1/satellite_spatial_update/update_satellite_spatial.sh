#!/bin/bash

# This script is needed for DR 2333 for GOES-R readiness.  The satellite spatial table needed to be refactored to store data to construct
# GridGeometry2D in CRS space instead of lat/lon due to the geostationary projection corner points not being valid in lat/lon space.

DIR=`dirname $0`
PSQL="/awips2/psql/bin/psql"
JAVA="/awips2/java/bin/java"

# Update columns on tables for satellite
${PSQL} -U awips -d metadata -c "ALTER TABLE satellite DROP COLUMN upperrightlat, DROP COLUMN upperrightlon, DROP COLUMN sizerecords, DROP COLUMN numrecords;"
${PSQL} -U awips -d metadata -c "ALTER TABLE satellite_spatial DROP COLUMN la1, DROP COLUMN la2, DROP COLUMN latin, DROP COLUMN lo1, DROP COLUMN lo2, DROP COLUMN lov;"
${PSQL} -U awips -d metadata -c "ALTER TABLE satellite_spatial ADD COLUMN minx double precision, ADD COLUMN miny double precision;"

# Run application to convert sat spatial entries to use crs space
${JAVA} -jar update_satellite_spatial.jar 
