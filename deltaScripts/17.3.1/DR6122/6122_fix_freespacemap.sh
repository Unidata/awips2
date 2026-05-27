#!/bin/bash

# #6122 - Repair any possible free-space map errors in PostgreSQL databases
# by deleting corrupt files, as described in 9.5.5 release notes:
# https://www.postgresql.org/docs/9.5/static/release-9-5-5.html
#
# If there are no errors to correct then this script does nothing except
# restart PostgreSQL.
#
# Run as root on all Postgres servers. Postgres must be running for this to
# work.
#
# Author: tgurney

psql=/awips2/psql/bin/psql
pg_ctl=/awips2/postgresql/bin/pg_ctl
data_dir=/awips2/data

su - awips -c "${pg_ctl} -D ${data_dir} -s status"
if [[ "$?" -ne 0 ]]; then
    echo "ERROR: Postgres is not running. Cannot continue."
    exit 1
fi

echo "INFO: Checking all databases for free-space map corruption"

filecount=0
for db in $(${psql} --user=awipsadmin --db=metadata -Aqt -c 'select datname from pg_database;'); do
    file_list=$(mktemp || exit 1)
    old_filecount=${filecount}
    echo -n "  ${db}..."
    # https://wiki.postgresql.org/wiki/Free_Space_Map_Problems
    ${psql} --user=awipsadmin --db=${db} -Aqt -c "
        create extension if not exists pg_freespacemap;
        select oid::regclass as relname,
            pg_relation_filepath(oid) || '_fsm' AS fsm
        from pg_class,
            cast(current_setting('block_size') as bigint) as bs
        where relkind in ('r', 'i', 't', 'm') and exists
        (select 1 from
        generate_series(pg_relation_size(oid) / bs,
                        (pg_relation_size(oid, 'fsm') - 2*bs) / 2) as blk
        where pg_freespace(oid, blk) > 0);
    " >> ${file_list} 2>/dev/null
    filecount=$(wc -l "${file_list}" | awk '{print $1}')
    if [[ "${filecount}" -gt "${old_filecount}" ]]; then
        echo "errors found"
    else
        echo "ok"
    fi
done


if [[ "${filecount}" -ne 0 ]]; then
    echo INFO: Need to delete ${filecount} corrupt files
    echo INFO: Stopping PostgreSQL
    su - awips -c "${pg_ctl} -D ${data_dir} -s stop"
    if [[ "$?" -ne 0 ]]; then
        echo ERROR: Failed to stop Postgres. Cannot continue
        rm -f ${file_list}
        exit 1
    fi
    while IFS= read -r line; do
        if [[ -f "${line}" ]]; then
            rm -fv ${line}
        fi
    done < ${file_list}
    echo INFO: Starting PostgreSQL
    su - awips -c "${pg_ctl} -D ${data_dir} -s start"
else
    echo "INFO: Found no free-space map errors"
fi

rm -f ${file_list}

echo INFO: Done.
