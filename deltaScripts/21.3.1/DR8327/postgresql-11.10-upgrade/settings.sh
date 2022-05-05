#!/bin/bash

# Settings for PostgreSQL major version upgrade scripts.
#
# Author: tgurney

# AWIPS II installation root
awips2=/awips2

if [[ -d "${awips2}/openfire_data" ]]; then
    is_openfire=1
else
    is_openfire=
fi


# Cluster data directory
if [[ -n "${is_openfire}" ]]; then
    postgres_data_dir=$awips2/openfire_data
else
    postgres_data_dir=$awips2/database/data
fi

# Default database to connect to after the cluster has been upgraded
if [[ -n "${is_openfire}" ]]; then
    default_db=openfire
else
    default_db=metadata
fi

# Database admin role name
if [[ -n "${is_openfire}" ]]; then
    db_admin_user=awips
else
    db_admin_user=awipsadmin
fi

# Old PostgreSQL version
old_ver="9.6.20"

# New PostgreSQL version
new_ver="11.10"

# PostgreSQL install prefix
postgres_dir=$awips2/postgresql

# PostgresSQL lib dir
postgres_lib="${postgres_dir}/lib"

# Actual currently installed PostgreSQL version
postgres_version=$($postgres_dir/bin/postgres --version | cut -d' ' -f3)

# Location where the old PostgreSQL installation has been copied to
postgres_copy_of_old=$awips2/postgresql-$old_ver

# psql install prefix
psql_dir=$awips2/psql

# psql binary
psql=$psql_dir/bin/psql

# Actual installed psql version
psql_version=$($psql --version | cut -d' ' -f3)

# Temp dir that will be created for working with the old cluster data dir
temp_old_data=$postgres_data_dir/.data-$old_ver

# Temp dir that will be created for working with the new cluster data dir
temp_new_data=$postgres_data_dir/.data-$new_ver

# Port to run the new cluster on during the upgrade process
temp_port=50432

# pg_upgrade binary
pg_upgrade=$postgres_dir/bin/pg_upgrade

# pg_tblspc binary
pg_tblspc=$postgres_data_dir/pg_tblspc

# list of config files in the cluster data dir that must be preserved
config_files=(pg_hba.conf pg_ident.conf postgresql.conf recovery.conf)

# initdb binary
initdb=$postgres_dir/bin/initdb

# vacuumdb binary
vacuumdb=$postgres_dir/bin/vacuumdb
