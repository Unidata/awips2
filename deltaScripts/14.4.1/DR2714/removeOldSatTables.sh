#!/bin/bash

# Omaha #2714 static tables have been replaced by XML backed in-memory lookup tables

DBUSER="awips"
DBNAME="metadata"

PSQL="/awips2/psql/bin/psql"

for table in satellite_creating_entities satellite_geostationary_positions satellite_physical_elements satellite_sector_ids satellite_sources satellite_units
do
    echo Dropping table: $table
    command="DROP TABLE $table"
    if ${PSQL} -U ${DBUSER} -d ${DBNAME} -c "$command"
    then
        echo $table dropped successfully
    else
        echo problem dropping table: $table
    fi
done 

echo Done
