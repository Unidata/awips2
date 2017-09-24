#!/bin/bash

# This script configures a server to allow Postgres replication:
# - Creates replication user
# - Adds lines to pg_hba.conf to allow replication
#
# This must run on all servers that will replicate or be replicated. You
# only need to run this once per server.

psql="/awips2/psql/bin/psql"
db_superuser=awips
postgres_data_dir=/awips2/data

cleanup_exit () {
    echo INFO: Cleaning up.
    rm -f ${temp_hba_conf}
    exit $1
}

temp_hba_conf=$(mktemp || cleanup_exit 1)

if [[ "$(id -u)" -ne 0 ]]; then
    echo ERROR: You need to be root.
    cleanup_exit 1
fi

echo "INFO: Creating replication role"

"${psql}" -v ON_ERROR_STOP=1 --user="${db_superuser}" --db=metadata << EOF || cleanup_exit 1
    begin transaction;
    drop role if exists replication;
    create role replication with replication login password 'replication';
    commit transaction;
EOF

grep -Ev "replication" "${postgres_data_dir}/pg_hba.conf" > ${temp_hba_conf}
cat << EOF >> ${temp_hba_conf} || cleanup_exit 1

# replication connections
local      replication replication                  trust
hostssl    replication replication 162.0.0.0/8      cert clientcert=1
hostssl    replication replication ::1/128          cert clientcert=1
EOF

echo INFO: Updating pg_hba.conf
install -T -m 600 -o awips -g fxalpha ${temp_hba_conf} "${postgres_data_dir}/pg_hba.conf" || cleanup_exit 1
echo "INFO: Finished. No errors reported."
cleanup_exit 0
