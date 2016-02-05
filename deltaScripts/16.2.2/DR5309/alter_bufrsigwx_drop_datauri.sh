#!/bin/bash
# DR #5309 - This script drops dataURI column from bufrsigwx table and adds a
#            new multi-column unique constraint

TABLE=bufrsigwx

PSQL="/awips2/psql/bin/psql"

echo "INFO: Altering table ${TABLE}"

${PSQL} -U awips -d metadata << EOF
begin transaction;
delete from ${TABLE}
    where wxLayer is null
    or wxType is null
    or key is null;
alter table ${TABLE}
    drop constraint if exists uk_${TABLE}_datauri_fields,
    drop column if exists datauri,
    alter wxLayer set not null,
    alter wxType set not null,
    alter key set not null,
    add constraint uk_${TABLE}_datauri_fields unique
        (reftime, wxLayer, wxType, key);
commit transaction;
EOF
