#!/bin/bash
# DR #5309 - This script drops dataURI column from bufrsigwx table and adds a
#            new multi-column unique constraint

TABLE=bufrsigwx
DBUSER=awips

PSQL="/awips2/psql/bin/psql"

echo "INFO: Altering table ${TABLE}"

${PSQL} -U ${DBUSER} -d metadata << EOF
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
        (reftime, forecasttime, wxLayer, wxType, key);
-- "bufrswigwx" not a typo. name was misspelled when the index was created.
drop index if exists bufrswigwx_refTimeIndex;
commit transaction;
EOF
