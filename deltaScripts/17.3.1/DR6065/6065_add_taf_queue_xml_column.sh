#!/bin/bash

# 6065 - This script creates the xml column on the taf_queue table.
#
# Author: tgurney
# Mar 22, 2017

psql=/awips2/psql/bin/psql

echo INFO: Adding xml column to taf_queue

exists=$(${psql} --user=awipsadmin --db=metadata -Atc "
    select exists(
        select 1 from information_schema.columns
        where table_name = 'taf_queue'
        and table_schema = 'awips'
        and table_catalog = 'metadata'
        and column_name = 'xml'
        );")

if [[ "${exists}" == "f" ]]; then
    ${psql} --user=awipsadmin --db=metadata -Atc "
        begin transaction;
        alter table if exists taf_queue add column xml boolean not null default false;
        alter table if exists taf_queue alter column xml drop default;
        commit transaction;
        "
elif [[ "${exists}" == "t" ]]; then
    echo WARN: Column already exists. Doing nothing
else
    echo ERROR: Failed to query the metadata database.
fi

echo INFO: Done.
