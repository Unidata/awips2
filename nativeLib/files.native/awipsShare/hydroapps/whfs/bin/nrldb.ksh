#!/usr/bin/ksh
###############################################################################
# AWIPS2 wrapper script for the daily scheduled NRLDB process.  This uses the #
# nrldb.pl script to extract the static data from the IHFSDB, packages it in  #
# an XML file and uploads it to the NRLDB server on the NHOR.                 #
#                                                                             #
# Mark Armstrong (HSD) - 10/17/2013                                           #
###############################################################################   
RUN_FROM_DIR=`dirname $0`
#echo "RFD: $RUN_FROM_DIR"
# set up SOME environment variables for WHFS applications
export PGSQL_DRIVER_DIR=/awips2/cave/plugins/org.postgres_9.2.0
export EDEX_HOME=/awips2/edex
export apps_dir=/awips2/edex/data/share/hydroapps
. $RUN_FROM_DIR/../../set_hydro_env
. $RUN_FROM_DIR/../../check_app_context


export NRLDB_DATA=$(get_apps_defaults nrldb_data)
#echo "NRLDB data: $NRLDB_DATA"

export NRLDB_LOG=$(get_apps_defaults nrldb_log)
#echo "NRLDB log: $NRLDB_LOG"

export NRLDB_CONFIG=$(get_apps_defaults nrldb_config)
#echo "NRLDB config: $NRLDB_CONFIG"

export WHFS_BIN=$(get_apps_defaults whfs_bin_dir)
#echo "WHFS_BIN: $WHFS_BIN"

export NRLDBLOGFILE=${NRLDB_LOG}/nrldb.log
export NRLDBTMPFILE=${NRLDB_LOG}/nrldb.tmp
tail -5000 $NRLDBLOGFILE > $NRLDBTMPFILE
mv $NRLDBTMPFILE $NRLDBLOGFILE

${WHFS_BIN}/nrldb.pl -t wfo -u

#
