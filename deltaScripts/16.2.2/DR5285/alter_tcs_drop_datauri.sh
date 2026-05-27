#!/bin/bash
# DR #5285 - This script drops dataURI column from tcs table and adds a new
#            multi-column unique constraint

PSQL="/awips2/psql/bin/psql"
DBUSER=awips

echo "INFO: Altering table tcs"

${PSQL} -U ${DBUSER} -d metadata << EOF
begin transaction;
alter table tcs
    drop constraint if exists uk_tcs_datauri_fields,
    drop column if exists datauri,
    add constraint uk_tcs_datauri_fields unique
        (reftime, forecasttime, producttype, latitude, longitude, stationid);
drop index if exists tcs_refTimeIndex;
commit transaction;
EOF
