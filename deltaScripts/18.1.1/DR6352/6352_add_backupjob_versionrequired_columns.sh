#!/bin/bash

# This script adds two columns 'minVersionRequired' and 'maxVersionRequired' to
# the backup_job table, with the current EDEX version as the default value for
# each

version=$(rpm -q awips2-database --qf %{VERSION})

if [[ $? -ne 0 ]]; then
    echo ERROR: Failed to get EDEX version. Cannot continue
    exit 1
fi

has_column() {
    table_name=${1}
    column_name=${2}
    result=$(psql --user=awipsadmin --db=metadata -Aqtc "
        select 1
        from information_schema.columns c
        where c.table_name = '${table_name}'
        and c.column_name='${column_name}';")
    [[ ${result} == "1" ]]
    return $?
}

has_column backup_job minversionrequired
if [[ $? -ne 0 ]]; then
    echo INFO: Adding column minversionrequired
    psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table backup_job add column minVersionRequired varchar(16);
        update backup_job set minVersionRequired = '${version}' where minVersionRequired is null;
        alter table backup_job alter column minVersionRequired set not null;
EOF
else
    echo INFO: minversionrequired column already exists.
fi

has_column backup_job maxversionrequired
if [[ $? -ne 0 ]]; then
    echo INFO: Adding column maxversionrequired
    psql --user=awipsadmin --db=metadata -1 << EOF
        \set ON_ERROR_STOP on
        alter table backup_job add column maxVersionRequired varchar(16);
        update backup_job set maxVersionRequired = '${version}' where maxVersionRequired is null;
        alter table backup_job alter column maxVersionRequired set not null;
EOF
else
    echo INFO: maxversionrequired column already exists.
fi
