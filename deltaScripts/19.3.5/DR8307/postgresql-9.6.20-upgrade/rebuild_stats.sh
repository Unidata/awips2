#!/bin/bash
# After a successful pg_upgrade, statistics are lost.
# Once the new cluster is running, run this script to regenerate the
# statistics.
#
# Author: tgurney

here="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source "${here}/settings.sh" || exit 1

databases=$("${psql}" --db $default_db -U $db_admin_user -Atc "
    select datname
    from pg_database
    where datistemplate = false
    and datname not in ('awips', 'postgres');
    ")

for dbname in ${databases}; do
    if [[ "$(id -u)" -eq 0 ]]; then
        sudo -u awips "${vacuumdb}"  --username $db_admin_user --analyze-in-stages ${dbname}
    else
        "${vacuumdb}" --username $db_admin_user --analyze-in-stages ${dbname}
    fi
done
