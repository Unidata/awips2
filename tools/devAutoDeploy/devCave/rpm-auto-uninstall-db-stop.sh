#!/bin/bash
#----------------------------------------------------------------------
# Auto script for a database uninstall
#----------------------------------------------------------------------
# Default values for these script variables may set using db.defaults
SOURCE_FOLDER=/awipscm/installers
LOGS_SAVE_DIR=/awipscm/EdexLogs

INSTALL_FOLDER=/awips2
PURGE_TIME=9

IHFS_DUMP_SCRIPT=/tmp/dump-ihfs.sh
DB_INSTALL_FOLDER=/awips2

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
# space separated -- may need others?
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
echo "Database Auto Installer software shutdown/uninstall Log For ${DATE}"
echo "**************************************************************************************"
HOST=`echo ${HOSTNAME} | cut -d. -f1`
if [ -f "${RUN_FROM_DIR}/db-defaults" ]; then
   echo "loading properties from 'db-defaults'"
   . ${RUN_FROM_DIR}/db-defaults
else
   echo "using default properties"
fi

echo "cd ${RUN_FROM_DIR}"
cd ${RUN_FROM_DIR}

echo "EDEX/Database installed in '${DB_INSTALL_FOLDER}'"

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

yum remove awips2-httpd-pypies -y
yum groupremove 'AWIPS II Database Server' -y
if [ -d ${INSTALL_FOLDER} ]; then
        echo "Cleaning up any left over files"
        rm -rf ${INSTALL_FOLDER}
else
        echo "Did not find ${INSTALL_FOLDER}"
fi

cd /awips2/edex/data/hdf5
rm -Rf /awips2/edex/data/hdf5
rm -Rf /awips2/
DATE=`date`

echo "------------------------------------------------------------"
echo "Auto Installer DB Stop and Uninstall Completed For ${HOST} At ${DATE}"
echo "-------------------------------------------------"

