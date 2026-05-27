#!/bin/bash
# DR #5335 - This script alters the multi-column unique constraint of the ccfp
#            table to include the rangestart and rangeend columns

TABLE=ccfp

PSQL="/awips2/psql/bin/psql"

echo "INFO: Altering table ${TABLE}"

${PSQL} -U awips -d metadata << EOF
begin transaction;
alter table ${TABLE}
    drop constraint if exists uk_${TABLE}_datauri_fields,
    add constraint uk_${TABLE}_datauri_fields unique
        (reftime, rangestart, rangeend, producttype, boxlat, boxlong);
commit transaction;
EOF
