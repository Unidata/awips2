#!/bin/bash

# Settings for PostGIS major version upgrade scripts.
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

# New PostGIS version, first two components of the version number
new_postgis_ver="3.3"

# New PostGIS version, full version number
new_postgis_ver_long="3.3.2"

# Directory where PostGIS SQL scripts have been installed
postgis_scripts_dir="/awips2/postgresql/share/contrib/postgis-${new_postgis_ver}"

# psql install prefix
psql_dir=$awips2/psql

# psql binary
psql=$psql_dir/bin/psql
