#!/bin/sh

## EDIT FROM HERE

# Use rpm to find installation locations / directory paths.
POSTGRESQL_INSTALL="/awips2"

# Installation prefix
prefix="${POSTGRESQL_INSTALL}/postgresql"

# Data Directory
PGDATA="${POSTGRESQL_INSTALL}/data"

# Where to keep a log file
PGLOG="$PGDATA/serverlog"

USER=`whoami`

## STOP EDITING HERE

# The path that is to be used for the script
PATH=${prefix}/bin:/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin

export LD_LIBRARY_PATH=${prefix}/lib

# What to use to start up the postmaster (we do NOT use pg_ctl for this,
# as it adds no value and can cause the postmaster to misrecognize a stale
# lock file)
DAEMON="$prefix/bin/postmaster"

# What to use to shut down the postmaster
PGCTL="$prefix/bin/pg_ctl"

set -e

# Only start if we can find the postmaster.
test -x $DAEMON || exit 0

echo -n "Starting PostgreSQL: "
$DAEMON -D $PGDATA



exit 0
