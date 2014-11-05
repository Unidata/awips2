#!/bin/bash
# DR #3788 - this update script will adjust the satellite_spatial to support sequenced gids.

PSQL="/awips2/psql/bin/psql"

echo "INFO: Updating satellite spatial table"

${PSQL} -U awips -d metadata -q -c "ALTER TABLE satellite_spatial DROP CONSTRAINT IF EXISTS uk_fdpq7gpkgi3r3k76j83x7axb1"
${PSQL} -U awips -d metadata -c "ALTER TABLE satellite_spatial ADD CONSTRAINT uk_fdpq7gpkgi3r3k76j83x7axb1 UNIQUE (minx, miny, dx, dy, nx, ny, crswkt)"
${PSQL} -U awips -d metadata -c "CREATE SEQUENCE satspatial_seq INCREMENT 1 MINVALUE 1 MAXVALUE 9223372036854775807 START 1 CACHE 1;"

echo "INFO: Satellite spatial table successfully updated."
