#!/bin/bash

# This script adds centralwavelength columns to the satellite table
# Run as root on Postgres server. Postgres must be running for this to
# work.
#
#
# Author: mgamazaychikov


echo INFO: Adding centralWavelength column to satellite table

has_column() {
    column_name=${1}
    result=$( /awips2/psql/bin/psql --user=awipsadmin --db=metadata -Aqtc "
        select 1
        FROM  information_schema.columns
        WHERE table_schema='awips' AND table_name='satellite' AND column_name='${column_name}'; " )

    [[ ${result} == "1" ]]
    return $?
}

add_central_wavelength() {
    has_column centralWavelength
    if [[ $? != 0 ]]; then
    echo INFO: Adding column centralWavelength to the awips.satellite table
        /awips2/psql/bin/psql --user=awipsadmin --db=metadata -1 << EOF
            \set ON_ERROR_STOP on
            ALTER TABLE awips.satellite ADD COLUMN centralWavelength real
EOF
    else
        echo INFO: centralWavelength column already exists in  the awips.satellite table
    fi
}

add_central_wavelength