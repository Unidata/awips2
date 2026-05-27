#!/bin/bash

# This script uninstalls PostGIS legacy functions from all Postgres databases.
# Run as root on all Postgres servers. Postgres must be running for this to
# work.
#
# Author: tgurney

psql=/awips2/psql/bin/psql

scripts_dir="/awips2/postgresql/share/contrib/postgis-2.2"

echo INFO: Starting PostGIS legacy function removal

databases=$(${psql} --db metadata -U awipsadmin -Atc "
    select datname
    from pg_database
    where datistemplate = false
    and datname not in ('awips', 'postgres');
    ")

if [[ -z "${databases}" ]]; then
    echo "ERROR: Failed to get a list of databases. Make sure postgres is running and pg_hba.conf is allowing connections."
    exit 1
fi

echo INFO: Found databases: ${databases}

for db in ${databases} ; do
    echo -n "  ${db}... "
    has_extension=$(${psql} --db ${db} -U awipsadmin -Atc "
        select 'y' from pg_extension where extname like 'postgis%' limit 1;
        ")
    if [[ "$?" -ne 0 ]]; then
        echo "ERROR: Failed to query the ${db} database. Make sure postgres is running and pg_hba.conf is allowing connections."
        continue
    fi
    if [[ "${has_extension}" == "y" ]]; then
        echo "found PostGIS"
        echo "INFO: Uninstalling legacy functions from ${db}"
        ${psql} --db "${db}" -U awipsadmin -f "${scripts_dir}"/uninstall_legacy.sql >/dev/null 2>&1
        echo "INFO: Done uninstalling legacy functions from ${db}"
    else
        echo "no PostGIS found."
    fi
done
echo INFO: Finished PostGIS legacy function removal
