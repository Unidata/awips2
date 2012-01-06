#!/bin/bash
#----------------------------------------------------------------------
# Auto deploy script for updating a database server. Where possible, machine
# dependent values are obtained at runtime.
#
#----------------------------------------------------------------------

LOGS_SAVE_DIR=/awipscm/EdexLogs
RUN_FROM_DIR=/awipscm/clusterDeployment
IHFS_DUMP_SCRIPT=/tmp/dump-ihfs.sh

#
#----------------------------------------------------------------------
# basic logic:
#   1) backs up key data
#   2) stops AWIPS II services
#   3) saves any logs
#   4) uninstalls AWIPS II software
#
# defaults:
#    default install works on awips-devdb
#
#----------------------------------------------------------------------
# args:
#  $1..$n :: components to stop/uninstall...
#
#----------------------------------------------------------------------
# limitations:
#   1) this script must be run as root
#
#----------------------------------------------------------------------
# space separated -- extensible framework
if [ $# -eq 0 ]; then
   INSTALLED_COMPONENTS="cli psql python"
else
   INSTALLED_COMPONENTS=${1}
   shift 1
   for a in $@; do
      INSTALLED_COMPONENTS="${INSTALLED_COMPONENTS} ${a}"
   done
fi

DATE=`date`
echo "**************************************************************************************"
echo "Database Auto Update software shutdown/update/startup for ${DATE}"
echo "**************************************************************************************"
HOST=`echo ${HOSTNAME} | cut -d. -f1`

echo "cd ${RUN_FROM_DIR}"
cd ${RUN_FROM_DIR}

echo "--------------------------------------------------------------------------------------"
echo "Stopping EDEX/Database on ${HOST} - ${DATE}"
echo "--------------------------------------------------------------------------------------"

echo "----Preparing to stop PostgreSQL----"
RUN_COUNT=`/bin/ps aux | /bin/grep postmaster | /bin/grep -v grep | /usr/bin/wc -l`
if [ ${RUN_COUNT} -eq 0 ]; then
   echo "   PostgreSQL does not appear to be running - continuing"
else
   echo "----Setting IHFS Vars----"
   POSTMASTER_STR=`ps ax -o user,args | grep postmaster | grep -v grep`
   PG_USER=`echo ${POSTMASTER_STR} | cut -d ' ' -f 1`
   PG_DATA_DIR=`echo ${POSTMASTER_STR} | cut -d ' ' -f 4`
   PG_PORT=`echo ${POSTMASTER_STR} | cut -d ' ' -f 6`
   echo "   PG user=${PG_USER}"
   echo "   PG data dir=${PG_DATA_DIR}"
   echo "   PG port=${PG_PORT}"

    IHFS=hd_ob83oax
    IHFS_SQL_FILE=/tmp/ihfs_data.sql

   echo "----Exporting IHFS Database----"
   if [ -f ${IHFS_DUMP_SCRIPT} ]; then
      # modified for ticket 1575 -- mwf
      echo "running ${IHFS_DUMP_SCRIPT} ${PG_USER} ${IHFS} ${IHFS_SQL_FILE} ${PG_PORT}"
      ${IHFS_DUMP_SCRIPT} ${PG_USER} ${IHFS} ${IHFS_SQL_FILE} ${PG_PORT}
      echo "----IHFS Export Complete----"
   else
      echo "   unable to find '${IHFS_DUMP_SCRIPT}' - continuing"
   fi
   DB_DAEMON_FILE="/etc/init.d/edex_postgres"
   if [ ! -f ${DB_DAEMON_FILE} ];then
      DB_DAEMON_FILE="${DB_DAEMON_FILE}_${LEGACY_DB_PORT}"
   fi
   if [ -f ${DB_DAEMON_FILE} ]; then
      echo "----Stopping Postgres----"
      echo "   running ${DB_DAEMON_FILE} stop"
      ${DB_DAEMON_FILE} stop
      echo "----Waiting ${PAUSE} seconds for the PostgreSQL process to stop---"
      sleep ${PAUSE}

      POSTMASTER_STR=`ps ax -o pid,args | grep postmaster | grep -v grep`
      PID=`echo ${POSTMASTER_STR} | cut -d ' ' -f 1`
      echo "Postmaster PID=${PID}"
      if [ "$PID" != "" ]; then
	     echo "----Found PostgreSQL PID killing ${PID}----"
	     echo "   kill -9 ${PID}"
	     kill -9 ${PID}
      else
	     echo "----No PostgreSQL PID found----"
      fi
   else
      echo "   unable to find shut down script ${DB_DAEMON_FILE} - will attempt to use kill"
      POSTMASTER_STR=`ps ax -o pid,args | grep postmaster | grep -v grep`
      PID=`echo ${POSTMASTER_STR} | cut -d ' ' -f 1`
      echo "   Killing Postmaster PID=${PID}"
      kill -9 ${PID}
   fi
   echo "----DB Services stopped----"

   service httpd-pypies stop
   echo "----Saving DB logs---"
   PG_LOG_DIR=${PG_DATA_DIR}/pg_log 
   if [ -d ${PG_LOG_DIR} ];then
      DATE_DIR=`date +"%m-%d-%y"`
      PG_LOG_ARCHIVE=${LOGS_SAVE_DIR}/${HOST}/${DATE_DIR}
if [ ! -d ${PG_LOG_ARCHIVE} ]; then
         echo "   creating archive dir ${PG_LOG_ARCHIVE}"
         mkdir -p ${PG_LOG_ARCHIVE}
      fi
      cp ${PG_LOG_DIR}/*.log ${PG_LOG_ARCHIVE}
   else
      echo "   unable to find any logs to copy"
   fi
   echo "----DB log save complete----"
fi

echo "----Waiting for the uninstaller to clean up the previous install.----"
TIMEOUT_COUNT=0

yum clean all
export http_proxy=
yum update 'AWIPS II Database Server' -y


chown -R awips:fxalpha /awips2/edex/data/*
chmod -Rf 775 /awips2/edex/data/*
cp /awipscm/clusterDeployment/bin/config.xml /awips2/rcm/data/config/persist/

service httpd-pypies start

echo "----Verifying database startup----"
POSTMASTER_STR=`ps ax -o pid,args | grep postmaster | grep -v grep`
PID=`echo ${POSTMASTER_STR} | cut -d ' ' -f 1`
if [ -n "${PID}" ]; then
   echo "    DB startup successful"
   echo "----Setting IHFS Vars----"
   POSTMASTER_STR=`ps ax -o user,args | grep postmaster | grep -v grep`
   PG_USER=`echo ${POSTMASTER_STR} | cut -d ' ' -f 1`
   PG_DATA_DIR=`echo ${POSTMASTER_STR} | cut -d ' ' -f 4`
   PG_PORT=`echo ${POSTMASTER_STR} | cut -d ' ' -f 6`
   PG_DAEMON=`echo ${POSTMASTER_STR} | cut -d ' ' -f 2`
   PG_BIN_DIR=`dirname ${PG_DAEMON}`
   export PATH=${PG_BIN_DIR}:${PATH}
   export LD_LIBRARY_PATH="${PG_BIN_DIR}/../lib:${PG_BIN_DIR}/../../psql/lib"
   echo "   PG user=${PG_USER}"
   echo "   PG data dir=${PG_DATA_DIR}"
   echo "   PG port=${PG_PORT}"
   echo "   PG bin dir=${PG_BIN_DIR}"

   if [ -f ${IHFS_SQL_FILE} ]; then
      echo "----Importing IHFS Data----"
      ${PG_BIN_DIR}/psql -q -U ${PG_USER} -p ${PG_PORT} -d ${IHFS} < ${IHFS_SQL_FILE} 
      echo "----IHFS Import Complete----"
   else
      echo "    Import IHFS Data older version. - ${IHFS_SQL_FILE} not found, continuing"
      IHFS_SQL_FILE=/awipscm/blichtenberg/ihfs_data.sql
      ${PG_BIN_DIR}/psql -q -U ${PG_USER} -p ${PG_PORT} -d ${IHFS} < ${IHFS_SQL_FILE} 
      let ERROR_COUNT=ERROR_COUNT+1
   fi
else
   echo "    WARNING: Unable to verify ${COMPONENT} startup - will attempt to continue"
   let ERROR_COUNT=ERROR_COUNT+1
fi


DATE=`date`

echo "------------------------------------------------------------"
echo "Auto Installer DB Stop and Uninstall Completed For ${HOST} At ${DATE}"
echo "-------------------------------------------------"

