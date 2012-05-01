#!/bin/bash
#----------------------------------------------------------------------
# auto deploy script for a database server. Where possible, machine
# dependent values are obtained at runtime.
#
#----------------------------------------------------------------------
# Default values for these script variables may set using db.defaults
MASTER_UNINSTALLER_JAR=awips-db-server-uninstaller.jar
DB_INSTALL_FOLDER=/dev-db
SOURCE_FOLDER=/awipscm/installers
IHFS_DUMP_SCRIPT=/installer/dump-ihfs.sh
IRT_PATH=/dev-db/irt/IRT-operational/server/
PYTHON_BIN_DIR=/awips/python/bin
LOGS_SAVE_DIR=/awipscm/EdexLogs
PAUSE=30
RUN_FROM_DIR=/installer
LEGACY_DB_PORT=5433

RADAR_SERVER=${DB_INSTALL_FOLDER}/rcm
RADAR_SERVER_BAK=/RadarServer-Bak

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

# determine if Radar Server is running -- if so, stop it
RUN_COUNT=`/bin/ps aux | /bin/grep java | /bin/grep rcm.mqsrvr | /usr/bin/wc -l`
if [ $RUN_COUNT -ne 0 ]; then
   echo "    stopping rcm..."
   echo "/etc/init.d/edex_rcm stop"
   /etc/init.d/edex_rcm stop
   sleep ${PAUSE}
else
   echo "    rcm not running, will continue"
fi

# determine if we failed to stop Radar Server, is so, kill it
echo "----Checking for any Radar Server processes that did not stop correctly----"
PID=`/sbin/pidof java`
while [ -n "${PID}" ]; do
   kill_count=0
   for a in ${PID}; do
      VAL=`ps ax -o pid,args | grep ${a} | grep "rcm.mqsrvr.MQServer"`
      if [ -n "${VAL}" ]; then
         echo "----Found Radar Server PID --- killing ${a} ----"
         echo "    kill -9 ${a}"
         kill -9 ${a}
         let kill_count=1
         sleep 10
      fi
   done
   if [ ${kill_count} -eq 0 ]; then
      echo ""
      echo "    no Radar Server processes found"
      PID=
   else
      PID=`/sbin/pidof java`
      echo -n "*"
   fi
done
echo ""

#check to see if IRT is running
echo "----Stopping IRT Server----"
if [ -d ${IRT_PATH} -a -f $IRT_PATH/irtd.pid ]; then
   IRT_PID=`cat $IRT_PATH/irtd.pid`
   COUNT=`ps ax -o pid | grep -c ${IRT_PID}`
   if [ ${COUNT} -ne 0 ]; then
      echo "   IRT running with pid '${IRT_PID}'"
      echo "/etc/init.d/edex_irt stop"
      /etc/init.d/edex_irt stop
      echo "---Waiting ${PAUSE} seconds for IRT processes to stop----"
      sleep ${PAUSE}
      echo "----Checking to see if the IRT Process has stopped----"
      IRT_PID=`/sbin/pidof ${PYTHON_BIN_DIR}/python RoutingTableSvc.py`
      if [ -n "$IRT_PID" ]; then
         echo "   IRT still running with PID '${IRT_PID}' - will kill"
         kill -9 ${IRT_PID}
      else
         echo "  no IRT pid found - continuing"
      fi
   else
      echo "   IRT Server not running - will continue"
   fi
else
   echo "   IRT Server not running - will continue"
fi

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
      echo "    running ${IHFS_DUMP_SCRIPT} ${PG_USER} ${IHFS} ${IHFS_SQL_FILE} ${PG_PORT}"
      ${IHFS_DUMP_SCRIPT} ${PG_USER} ${IHFS} ${IHFS_SQL_FILE} ${PG_PORT}
      echo "----IHFS Export Complete----"
   else
      echo "   unable to find '${IHFS_DUMP_SCRIPT}' - continuing"
   fi

   # added for ticket 3755
#   SUBSCRIPTION_SCRIPT=/installer/dump-subscription.sh
#   SUBSCRIPTION_DDL_FILE=/tmp/subscription_data.sql
#   echo "----Exporting Subscription Tables----"
#   if [ -f ${SUBSCRIPTION_SCRIPT} ]; then
#      echo "    running ${SUBSCRIPTION_SCRIPT} ${SUBSCRIPTION_DDL_FILE}"
#      ${SUBSCRIPTION_SCRIPT} ${SUBSCRIPTION_DDL_FILE}
#      echo "----Subscription Export Complete----"
#   else
#      echo "   unable to find '${SUBSCRIPTION_SCRIPT}' - continuing"
#   if

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

   echo "----Saving DB logs---"
   PG_LOG_DIR=${PG_DATA_DIR}/pg_log 
   if [ -d ${PG_LOG_DIR} ];then
      DATE_DIR=`date +"%m-%d-%y"`
      HOST_STR=`echo ${HOST} |cut -d- -f 2`
      PG_LOG_ARCHIVE=${LOGS_SAVE_DIR}/${HOST_STR}/${DATE_DIR}
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

# backup Radar Server if installed
echo "----Backing up Radar Server data----"
if [ -d ${RADAR_SERVER} ];then
   if [ -d ${RADAR_SERVER_BAK} ];then
      echo "    deleting previous backup"
      rm -rf ${RADAR_SERVER_BAK}
   fi
   echo "    moving existing Radar Server to backup"
   mv ${RADAR_SERVER} ${RADAR_SERVER_BAK}
fi
echo "----Radar Server backup complete----"

echo "----Uninstalling EDEX/Database software----"
echo "   components: ${INSTALLED_COMPONENTS}"
# this will need to be more selective of the deletes
# note: under the component installer model, we need
#       to uninstall the components individually. Once 
#       that is done, we can uninstall the overall
#       installer scripts.  This will also clean up
#       any leftover directories...
# As a further complication, the IzPak uninstaller is
# written so that it runs completely asynchronistically;
# this means we can't simply wait as the uninstaller runs...

echo "Removing Directories ..."
rm -rf ${DB_INSTALL_FOLDER}

echo "-----Running uninstallers-----"
if [ -f ${DB_INSTALL_FOLDER}/Uninstaller/uninstaller.jar ]; then
   # use the legacy uninstaller if it exists
   # -- note this is hard coded to the path
   # --      this branch is provided to allow for an automatic
   # --      switch to the new installers 
   echo "   running legacy uninstaller"   
   echo "   /installer/install_files/jre/bin/java -jar $DB_INSTALL_FOLDER/Uninstaller/uninstaller.jar -c -f"
   /installer/install_files/jre/bin/java -jar $DB_INSTALL_FOLDER/Uninstaller/uninstaller.jar -c -f
else
   # no legacy uninstaller, delete components
   # note do not use force on the components 
   #  -- server uninstaller will do any additional cleanup
   # then delete the group installer scripts and clean up
   MASTER_UNINSTALLER=${DB_INSTALL_FOLDER}/Uninstaller/${MASTER_UNINSTALLER_JAR}
   BASE_NAME=`basename ${MASTER_UNINSTALLER}`
   echo "    Running server uninstaller ${BASE_NAME}"
   # note: use force on the bundle uninstaller
   if [ -f ${MASTER_UNINSTALLER} ]; then
      echo "   ${SOURCE_FOLDER}/install_files/jre/bin/java -jar ${MASTER_UNINSTALLER} -c -f"
      ${SOURCE_FOLDER}/install_files/jre/bin/java -jar ${MASTER_UNINSTALLER} -c -f
   fi
fi

echo "----Waiting for the uninstaller to clean up the previous install.----"
TIMEOUT_COUNT=0
while [ -d $DB_INSTALL_FOLDER -a $TIMEOUT_COUNT -lt 100 ]
do
  sleep 5
  let TIMEOUT_COUNT=TIMEOUT_COUNT+1 
  echo "----${DB_INSTALL_FOLDER} folder still exists; pausing. $TIMEOUT_COUNT----"
done

if [ $TIMEOUT_COUNT = 100 ]
then
  	echo "****TIMED OUT WAITING FOR UNINSTALLER TO DELETE ${DB_INSTALL_FOLDER} FOLDER...****"


	echo "----Deleting the ${DB_INSTALL_FOLDER} directory----"
	rm -rf ${DB_INSTALL_FOLDER}

	echo "----Sending Warning Email----"
	echo "`uname -n`: At `date`, Dev Autodeployment script failed to stop Edex/database normally--Uninstall was killed and ${DB_INSTALL_FOLDER} directory was deleted manually, before continuing with the install--${HOST} should be checked." > email.txt
#	mail -s "Warning Dev Autodeployment uninstall failed on `uname -n`" Kace_Chrisman@raytheon.com < email.txt
#	mail -s "Warning Dev Autodeployment uninstall failed on `uname -n`" mark_w_fegan@raytheon.com < email.txt
#	mail -s "Warning Dev Autodeployment uninstall failed on `uname -n`" Bryan_J_Rockwood@raytheon.com < email.txt
#	mail -s "Warning Dev Autodeployment uninstall failed on `uname -n`" Scott_Risch@raytheon.com < email.txt
        mail -s "Warning Dev Autodeployment uninstall failed on `uname -n`" Bryan.Kowal@raytheon.com < email.txt
fi


DATE=`date`

echo "--------------------------------------------------------------------------------------"
echo "Auto Installer DB Stop and Uninstall Completed For ${HOST} At ${DATE}"
echo "--------------------------------------------------------------------------------------"

