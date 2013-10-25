#!/bin/ksh

RUN_FROM_DIR=`dirname $0`
# set up SOME environment variables for WHFS applications
export PGSQL_DRIVER_DIR=/awips2/cave/plugins/org.postgres_9.2.0
. $RUN_FROM_DIR/../../set_hydro_env
. $RUN_FROM_DIR/../../check_app_context
export APPS_DEFAULTS=~/caveData/common/base/hydro/Apps_defaults
export APPS_DEFAULTS_SITE=~/caveData/common/site/${AW_SITE_IDENTIFIER}/hydro/Apps_defaults
export PGUSER="awips"
export BIN_DIR=`get_apps_defaults whfs_bin_dir"`
export NRLDB_LOG=`get_apps_defaults nrldb_log`
export NRLDB_CONFIG=`get_apps_defaults nrldb_config`
export NRLDB_DATA=`get_apps_defaults nrldb_data`
export NRLDB_TMP=`get_apps_defaults nrldb_tmp`
export db_name=`get_apps_defaults db_name`
export PGHOST=`get_apps_defaults pghost`

$RUN_FROM_DIR/update_nrldb.pl

exit 0
