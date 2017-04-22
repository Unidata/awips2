#!/bin/bash

# DR #5885: This script configures a server for Postgres replication.
# It must run on all Postgres servers that will replicate or be replicated.

psql="/awips2/psql/bin/psql"
db_superuser=awips
postgres_data_dir=/awips2/data

cleanup_exit () {
    echo INFO: Cleaning up.
    rm -f ${temp_postgresql_conf}
    rm -f ${temp_hba_conf}
    exit $1
}

temp_hba_conf=$(mktemp || cleanup_exit 1)
temp_postgresql_conf=$(mktemp || cleanup_exit 1)

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
local   replication replication                     trust
host    replication replication 127.0.0.1/32          md5
host    replication replication 162.0.0.0/8           md5
host    replication replication ::1/128               md5
EOF

cat "${postgres_data_dir}/postgresql.conf" \
  | perl -pe 's/^\#?\s*max_wal_senders\s*=.*$/max_wal_senders = 5/g' \
  | perl -pe 's/^\#?\s*wal_keep_segments\s*=.*$/wal_keep_segments = 64/g' \
  | perl -pe 's/^\#?\s*hot_standby\s*=.*$/hot_standby = on/g' \
  | perl -pe 's/^\#?\s*wal_level\s*=.*$/wal_level = hot_standby/g' \
  > ${temp_postgresql_conf} || cleanup_exit 1

echo INFO: Updating postgresql.conf
install -T -m 600 -o awips -g fxalpha ${temp_postgresql_conf} "${postgres_data_dir}/postgresql.conf" || cleanup_exit 1
echo INFO: Updating pg_hba.conf
install -T -m 600 -o awips -g fxalpha ${temp_hba_conf} "${postgres_data_dir}/pg_hba.conf" || cleanup_exit 1
echo "INFO: Finished. No errors reported."
cleanup_exit 0
