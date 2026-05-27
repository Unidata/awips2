#!/bin/bash

# This script just creates a PostgreSQL role called '*.awips.noaa.gov', destroying
# and re-creating the role if it already exists.
# The role name has to match the subject name in the certificate, *.awips.noaa.gov
# is the subject name in the wildcard dod cert installed in postgresql.

#
# Author: tgurney

psql=". /awips2/etc/environment; a2dbauth /awips2/psql/bin/psql -h $(hostname -s)"

if [[ "$(id -u)" -ne 0 ]]; then
    echo ERROR: You need to be root.
    exit 1
fi

echo "INFO: Creating replication role"

sudo -u awips -i "${psql}" -v ON_ERROR_STOP=1 --user=awipsadmin --db=metadata << EOF || exit 1
    begin transaction;
    drop role if exists '*.awips.noaa.gov';
    create role '*.awips.noaa.gov' with replication login password 'replication';
    commit transaction;
EOF

echo "INFO: Finished. No errors reported."
