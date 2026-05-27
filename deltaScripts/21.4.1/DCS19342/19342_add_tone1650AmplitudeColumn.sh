#!/bin/bash

# This script adds tone1650amplitude column to the transmitter_group table
# Run as root on Postgres server. Postgres must be running for this to
# work.
#
#
# Author: mgamazaychikov


has_column() {
    db=${1}
    column_name=${2}
    result=$( /awips2/psql/bin/psql --user=awipsadmin --db=${db} -Aqtc "
        select 1
        FROM  information_schema.columns
        WHERE table_schema='public' AND table_name='transmitter_group' AND column_name='${column_name}'; " )

    [[ ${result} == "1" ]]
    return $?
}

add_tone1650amplitude() {
    db=${1}
    has_column ${db} tone1650amplitude
    if [[ $? != 0 ]]; then
        /awips2/psql/bin/psql --user=awipsadmin --db=${db} -1 << EOF
            \set ON_ERROR_STOP on
            ALTER TABLE public.transmitter_group ADD COLUMN tone1650amplitude SMALLINT NOT NULL DEFAULT 12214;
            ALTER TABLE public.transmitter_group ALTER COLUMN tone1650amplitude DROP DEFAULT
EOF
        echo INFO: Added column tone1650amplitude to db ${db} transmitter_group table 
    else
        echo INFO: tone1650amplitude column already exists in db ${db} transmitter_group table
    fi
}

add_tone1650amplitude bmh
add_tone1650amplitude bmh_practice
