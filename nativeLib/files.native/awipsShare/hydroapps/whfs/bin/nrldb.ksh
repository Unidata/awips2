#!/usr/bin/ksh

#setenv FXA_HOME /awips/fxa
#setenv LOG_DIR /data/logs/fxa
#source $FXA_HOME/readenv.csh

RUN_FROM_DIR=`dirname $0`
echo "RFD: $RUN_FROM_DIR"
# set up SOME environment variables for WHFS applications
. $RUN_FROM_DIR/../../set_hydro_env
. $RUN_FROM_DIR/../../check_app_context

#set NRLDB_DATA=`/awips/hydroapps/public/bin/get_apps_defaults.LX nrldb_data`
#set NRLDB_LOG=`/awips/hydroapps/public/bin/get_apps_defaults.LX nrldb_log`
#set NRLDB_CONFIG=`/awips/hydroapps/public/bin/get_apps_defaults.LX nrldb_config`
#set WHFS_BIN=`/awips/hydroapps/public/bin/get_apps_defaults.LX whfs_bin_dir`
#cd /awips/hydroapps/whfs/local/data/backup_db/nrldb

export NRLDB_DATA=$(get_apps_defaults nrldb_data)
echo "NRLDB data: $NRLDB_DATA"

export NRLDB_LOG=$(get_apps_defaults nrldb_log)
echo "NRLDB log: $NRLDB_LOG"

export NRLDB_CONFIG=$(get_apps_defaults nrldb_config)
echo "NRLDB config: $NRLDB_CONFIG"

export WHFS_BIN=$(get_apps_defaults whfs_bin_dir)
echo "WHFS_BIN: $WHFS_BIN"

export NRLDBLOGFILE=${NRLDB_LOG}/nrldb.log
export NRLDBTMPFILE=${NRLDB_LOG}/nrldb.tmp
tail -5000 $NRLDBLOGFILE > $NRLDBTMPFILE
mv $NRLDBTMPFILE $NRLDBLOGFILE

${WHFS_BIN}/nrldb.pl -t wfo -u

#
