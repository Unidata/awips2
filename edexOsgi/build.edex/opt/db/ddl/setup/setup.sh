#!/bin/bash

#
# Initial setup script for the AWIPS II PostgreSQL database cluster.
# This is only meant to run once, as a post-install step for the
# awips2-database package.
#
# IMPORTANT NOTE: If PostgreSQL is already running when this script starts,
# PostgreSQL will be stopped (via systemctl) and the /awips2/database/data and
# /awips2/tablespace directories will be erased before continuing execution of
# this script. When this script terminates, PostgreSQL will remain stopped.
#
# Date         Ticket#    Engineer    Description
# ------------ ---------- ----------- --------------------------
# Jul  7, 2021 8544       tgurney     Initial creation, adapted from %post
#                                     section of awips2-database spec file
#
#

if [[ "$(id --user)" -ne 0 ]]; then
    echo "$0: ERROR: You need to be root to run this script, not $(whoami)"
    exit 1
fi

set -o pipefail

# Have to keep these two variables around for compatibility. We call some
# scripts that require them as arguments
postgresql_install="/usr"
psql_install="/usr"

# This is filled in automatically later
tmpdir=

data_dir="/awips2/database/data"
tablespace_dir="/awips2/database/tablespaces"
sql_share_dir="/awips2/database/sqlScripts/share/sql"
sql_log="${sql_share_dir}/sql_install.log"
db_owner="awips"
db_user="awips"
db_admin_user="awipsadmin"
db_owner_group="fxalpha"
db_port="5432"

A2LIBS=1 source /etc/profile.d/awips2PSQL.sh || exit 1

function cleanup() {
    trap - SIGINT SIGTERM
    echo "Cleaning up"
    systemctl stop postgresql@awips
    if [[ "$tmpdir" != "" && -f "${tmpdir}/postgresql.conf" ]]; then
        mkdir --parents /awips2/database/data
        mv "${tmpdir}/postgresql.conf" /awips2/database/data/
    fi
    rm --recursive --force "$tmpdir"
}

function die() {
    cleanup
    exit 1
}

function create_dir_for_tablespace() {
    install --directory \
        --owner=$db_owner \
        --group=$db_owner_group \
        "${tablespace_dir}/${1}"
}

function set_tablespace_dir_in_script() {
    # Escape forward slashes
    tablespace_dir_escaped=$(sed 's/\//\\\//g' <<< "$tablespace_dir")
    sed --in-place "s/%{tablespace_dir}%/${tablespace_dir_escaped}/g" "$1"
}

function run_sql_script() {
   # $1 == script to execute
   # $2 == database to connect to
   if [[ "$3" == "" ]]; then
       run_as="${db_admin_user}"
   else
       run_as="${3}"
   fi
   sudo --user="${db_user}" --non-interactive --preserve-env=PGHOST \
      psql \
      --db="$2" \
      --user="${run_as}" \
      --port="${db_port}" \
      --file="$1" \
      2>&1 | tee --append "${sql_log}"
}

trap die SIGINT SIGTERM

tmpdir="$(mktemp --directory)" || die
chmod 700 "$tmpdir" || die

echo "Stopping PostgreSQL if it is running"
systemctl stop postgresql@awips
if systemctl status postgresql@awips; then
    echo "ERROR: Failed to stop PostgreSQL. Cannot continue."
    die
fi

# data_dir was supposed to be set earlier. But just to be extra safe:
if [[ "$data_dir" == "" ]]; then
    echo "$0: ERROR: data_dir variable is empty. Cannot continue"
    die
fi

echo "Backing up postgresql.conf"
mv "${data_dir}"/postgresql.conf "${tmpdir}/" || die
rm --recursive --force "${data_dir}"/*
rm --recursive --force "${tablespace_dir}"/*

echo "Running initdb"
sudo --user="${db_user}" --non-interactive --preserve-env=PGHOST \
    initdb \
    --auth=trust \
    --locale=en_US.UTF-8 \
    --pgdata="${data_dir}" \
    --lc-collate=en_US.UTF-8 \
    --lc-ctype=en_US.UTF-8 \
    || die

echo "Restoring postgresql.conf"
mv "${tmpdir}/postgresql.conf" /awips2/database/data || die

echo "Creating tablespace directories"
create_dir_for_tablespace metadata || die
create_dir_for_tablespace pgdata_ihfs || die
create_dir_for_tablespace damcat || die
create_dir_for_tablespace hmdb || die
create_dir_for_tablespace climate || die

echo "Starting PostgreSQL"
systemctl start postgresql@awips || die

echo "Running initial setup SQL script"
initial_sql_script="${sql_share_dir}/initial_setup_server.sql"
sed --in-place "s/%{databaseUsername}/${db_user}/g" "${initial_sql_script}" || die
set_tablespace_dir_in_script "${initial_sql_script}" || die
run_sql_script "${initial_sql_script}" postgres "${db_user}" || die

echo "Creating PostGIS extensions"
psql --user=awipsadmin --db=metadata \
    --command="create extension if not exists postgis;" || die
psql --user=awipsadmin --db=metadata \
    --command="create extension if not exists postgis_raster;" || die
psql --user=awipsadmin --db=metadata \
    --command="create extension if not exists postgis_topology;" || die

echo "Running permissions.sql"
run_sql_script ${sql_share_dir}/permissions.sql metadata || die

echo "Running fxatext.sql"
run_sql_script ${sql_share_dir}/fxatext.sql metadata || die

echo "Running createEventsSchema.sql"
run_sql_script ${sql_share_dir}/createEventsSchema.sql metadata || die

echo "Creating climate DB"
set_tablespace_dir_in_script "${sql_share_dir}/createClimateDb.sql" || die
sudo --user="${db_user}" --non-interactive --preserve-env=PGHOST \
   "${sql_share_dir}/createClimateDB.sh" \
    "${psql_install}" \
    "${postgresql_install}" \
    "${db_port}" \
    "${db_admin_user}" \
    "${sql_share_dir}" \
    "${sql_log}" \
    || die

echo "Creating EBXML schema"
run_sql_script ${sql_share_dir}/createEbxml.sql metadata || die

# install replication role if this is a central registry server
source /awips2/database/.global 2>/dev/null
if [[ -e /data/fxa/INSTALL/awips2/scripts/.global ]]; then
    source /data/fxa/INSTALL/awips2/scripts/.global
fi

case $SITE_IDENTIFIER in
    ${centralCaseArray} )
        run_sql_script "${sql_share_dir}/createReplicationRole.sql" metadata || die
        ;;
    *)  ;;
esac

rm --force /awips2/database/.global

echo "Setting up roles and permissions"
"${sql_share_dir}/alter_database_roles_and_permissions.sh" || die

echo "Stopping PostgreSQL"
systemctl stop postgresql@awips

# Finally install the pg_hba.conf. Installing it earlier could prevent
# necessary database connections during the setup process
echo "Installing pg_hba.conf"
rm --force "${data_dir}/pg_hba.conf"
cp "${sql_share_dir}/pg_hba.conf" "${data_dir}/pg_hba.conf" || die
chown $db_owner:$db_owner_group "${data_dir}/pg_hba.conf" || die

cleanup
