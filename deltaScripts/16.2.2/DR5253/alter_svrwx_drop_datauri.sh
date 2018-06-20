#!/bin/bash
# DR #5253 - This script drops dataURI column from svrwx table and adds a new
#            multi-column unique constraint

PSQL="/awips2/psql/bin/psql"

echo "INFO: Altering table svrwx"

${PSQL} -U awips -d metadata << EOF
begin transaction;
alter table svrwx
    drop constraint if exists uk_svrwx_datauri_fields,
    drop column if exists datauri,
    add constraint uk_svrwx_datauri_fields unique (
        reftime, reporttype, stationid, latitude, longitude);
commit transaction;
EOF
