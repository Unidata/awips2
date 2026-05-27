#!/bin/bash

# This script adds the delta_time and scan type columns to the
# radar metadata database
# Run as root on Postgres server. Postgres must be running for this to
# work.
#
#
# Author: mroos, modified from: skabasele


echo INFO: Adding deltaTime column and scanType columns

has_column() {
    column_name=${1}
    result=$( /awips2/psql/bin/psql --user=awipsadmin --db=metadata -Aqtc "
        select 1
        FROM  information_schema.columns
        WHERE table_schema='awips' AND table_name='radar' AND column_name='${column_name}'; " )

    [[ ${result} == "1" ]]
    return $?
}


add_scan_type() {
    has_column scanType
    if [[ $? != 0 ]]; then
    echo INFO: Adding column scanType to the awips.radar table
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
            ALTER TABLE awips.radar ADD COLUMN scanType varchar(16) DEFAULT 'NORMAL'
EOF
    else
        echo INFO: scanType column already exists in  the awips.radar table
    fi
}

add_delta_time() {
    has_column deltaTime
    if [[ $? != 0 ]]; then
    echo INFO: Adding column deltaTime to the awips.radar table
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
                ALTER TABLE awips.radar ADD COLUMN deltaTime SMALLINT DEFAULT 0
EOF
    else
        echo INFO: deltaTime column already exists in  the awips.radar table
    fi
}

add_delta_time
add_scan_type
