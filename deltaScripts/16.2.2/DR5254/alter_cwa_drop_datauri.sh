#!/bin/bash
# DR #5254 - This script drops dataURI column from cwa table and adds a new
#            multi-column unique constraint

PSQL="/awips2/psql/bin/psql"

echo "INFO: Altering table cwa"

${PSQL} -U awips -d metadata << EOF
begin transaction;
delete from cwa where eventid is null;
alter table cwa
    drop constraint if exists uk_cwa_datauri_fields,
    drop column if exists datauri,
    alter eventid set not null,
    add constraint uk_cwa_datauri_fields unique (reftime, eventid);
commit transaction;
EOF
