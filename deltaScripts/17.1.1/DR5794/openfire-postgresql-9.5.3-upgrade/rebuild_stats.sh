#!/bin/bash
# After pg_upgrade from 9.3.x to 9.5.x, statistics are lost.
# Once the new 9.5.x cluster is running, run this script to regenerate
# the statistics.

VACUUMDB=/awips2/postgresql/bin/vacuumdb
PSQL=/awips2/psql/bin/psql
DATABASES=$(${PSQL} --db openfire -U awips -Atc "
    select datname
    from pg_database
    where datistemplate = false
    and datname not in ('awips', 'postgres');
    ")

for DBNAME in ${DATABASES}; do
    if [[ "$(id -u)" -eq 0 ]]; then
        sudo -u awips ${VACUUMDB}  --username awips --analyze-in-stages ${DBNAME}
    else
        ${VACUUMDB} --username awips --analyze-in-stages ${DBNAME}
    fi
done
