#!/bin/bash
# DR #5285 - This script drops dataURI column from tcs table and adds a new
#            multi-column unique constraint

PSQL="/awips2/psql/bin/psql"

echo "INFO: Altering table tcs"

${PSQL} -U awips -d metadata << EOF
begin transaction;
alter table tcs
    drop constraint if exists uk_tcs_datauri_fields,
    drop column if exists datauri,
    add constraint uk_tcs_datauri_fields unique
        (reftime, producttype, latitude, longitude, stationid);
commit transaction;
EOF
