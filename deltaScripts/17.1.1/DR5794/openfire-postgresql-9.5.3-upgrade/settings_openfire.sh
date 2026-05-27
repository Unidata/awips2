#!/bin/bash
# Settings for PostgreSQL major version upgrade scripts.
# Specifically for upgrading an Openfire cluster.

OLD_VER="9.3.10"
NEW_VER="9.5.3"
AWIPS2=/awips2
POSTGRES_DIR=$AWIPS2/postgresql
POSTGRES_VERSION=$($POSTGRES_DIR/bin/postgres --version | cut -d' ' -f3)
POSTGRES_COPY_OF_OLD=$AWIPS2/postgresql-$OLD_VER
POSTGRES_DATA_DIR=$AWIPS2/openfire_data
PSQL_DIR=$AWIPS2/psql
PSQL=$PSQL_DIR/bin/psql
PSQL_VERSION=$($PSQL --version | cut -d' ' -f3)
TEMP_OLD_DATA=$POSTGRES_DATA_DIR/.data-$OLD_VER
TEMP_NEW_DATA=$POSTGRES_DATA_DIR/.data-$NEW_VER
TEMP_PORT=50432
PG_UPGRADE=$POSTGRES_DIR/bin/pg_upgrade
PG_TBLSPC=$POSTGRES_DATA_DIR/pg_tblspc
CONFIG_FILES=(pg_hba.conf pg_ident.conf postgresql.conf)
INITDB=$POSTGRES_DIR/bin/initdb
