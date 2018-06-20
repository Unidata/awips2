#!/bin/bash
# DR #5310 - This script drops dataURI column from bufrncwf table and adds a
#            new multi-column unique constraint

TABLE=bufrncwf

PSQL="/awips2/psql/bin/psql"

echo "INFO: Altering table ${TABLE}"

${PSQL} -U awips -d metadata << EOF
begin transaction;
alter table ${TABLE}
    drop constraint if exists uk_${TABLE}_datauri_fields,
    drop column if exists datauri,
    add constraint uk_${TABLE}_datauri_fields unique
        (reftime, latitude, longitude, stationid);
commit transaction;
EOF
