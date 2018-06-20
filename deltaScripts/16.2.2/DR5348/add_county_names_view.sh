#!/bin/sh 
# DR #5348 - This script creates the county_names view on the county table
#            in the maps database

if [ -z $1 ] ; then
    PGHOST=dx1
else
    PGHOST=${1}
fi

/awips2/psql/bin/psql -d maps -U awips -q -h ${PGHOST} -c "
    DROP VIEW IF EXISTS mapdata.county_names;
    CREATE OR REPLACE VIEW mapdata.county_names AS
    SELECT countyname as name, ST_SetSRID(ST_Point(lon,lat), 4326)::geometry(Point, 4326) as the_geom
    FROM mapdata.county;
"
