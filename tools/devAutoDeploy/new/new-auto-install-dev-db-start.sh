#!/bin/bash
#----------------------------------------------------------------------
# auto deploy script for a database server. Where possible, machine
# dependent values are obtained at runtime.
#
#----------------------------------------------------------------------
# Default values for these script variables may set using 'db.defaults'
SOURCE_FOLDER=/awipscm/installers
DB_INSTALLER_SCRIPT=awips-db-server-setup.sh
IZ_INSTALLER_SCRIPT=db-server-install-script.xml
RUN_FROM_DIR=/installer
PURGE_TIME=9
DB_INSTALL_FOLDER=/dev-db

RADAR_SERVER=${DB_INSTALL_FOLDER}/rcm
RADAR_SERVER_BAK=/RadarServer-Bak

#
#---------------------------------------------------------------------
# basic logic:
#   1) Installs new AWIPS II software
#   2) starts AWIPS II services
#   3) imports previously saved data
#
# defaults:
#    default install works on awips-devdb
#
#----------------------------------------------------------------------
# args:
#  $1..$n :: components to start...
#
#----------------------------------------------------------------------
# limitations:
#   1) this script must be run as root
#   2) must run new-copy-installer-files.sh prior to running this script?
#
#----------------------------------------------------------------------
# space separated -- may need others?
if [ $# -eq 0 ]; then
   INSTALLED_COMPONENTS="postgres"
else
   INSTALLED_COMPONENTS=${1}
   shift 1
   for a in $@; do
      INSTALLED_COMPONENTS="${INSTALLED_COMPONENTS} ${a}"
   done
fi

echo "cd ${RUN_FROM_DIR}"
cd ${RUN_FROM_DIR}

HOST=`echo $HOSTNAME | cut -d. -f1`
DATE=`date`
echo "**************************************************************************************"
echo "Database Auto Installer software install/startup Log For $DATE"
echo "**************************************************************************************"
if [ -f "${RUN_FROM_DIR}/db-defaults" ]; then
   echo "loading properties from 'db-defaults'"
  . ${RUN_FROM_DIR}/db-defaults
else
   echo "using default properties"
fi
echo "--------------------------------------------------------------------------------------"
echo "Starting EDEX/Database on ${HOST} - ${DATE}"
echo "--------------------------------------------------------------------------------------"

echo "----Checking to make sure we can see the installer script----"

INSTALLER_SCRIPT=${SOURCE_FOLDER}/${DB_INSTALLER_SCRIPT}
echo ${INSTALLER_SCRIPT}
ls -l ${INSTALLER_SCRIPT}
if [ -f "${INSTALLER_SCRIPT}" ]
then
	echo "----Installer script ${DB_INSTALLER_SCRIPT} is available----"
else
	echo "----${DB_INSTALLER_SCRIPT} does not exist.  Stopping the automated installation----"
	echo "`uname -n`: At `date`, Dev Autodeployment script failed-nightly installer file does not exist" > email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" Kace_Chrisman@raytheon.com < email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" mark_w_fegan@raytheon.com < email.txt
        mail -s "Dev Autodeployment failed on `uname -n`" Bryan.Kowal@raytheon.com < email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" Bryan_J_Rockwood@raytheon.com < email.txt
	mail -s "Dev Autodeployment failed on `uname -n`" Scott_Risch@raytheon.com < email.txt
    exit
fi

echo "----Starting the installer script ${DB_INSTALLER_SCRIPT}----"
echo "    Executing ${INSTALLER_SCRIPT}"
echo "    Using auto-deploy script '${IZ_INSTALLER_SCRIPT}'"
${INSTALLER_SCRIPT} ${RUN_FROM_DIR}/${IZ_INSTALLER_SCRIPT}

echo "----Software Install complete.  Starting the EDEX DB services----"
ERROR_COUNT=0
for COMPONENT in ${INSTALLED_COMPONENTS}; do
   echo "----Starting ${COMPONENT}----"
   DAEMON="/etc/init.d/edex_${COMPONENT}"
   if [ -f ${DAEMON} ]; then
      echo "    executing ${DAEMON} start"
      ${DAEMON} start
      echo "----Verifying ${DAEMON} startup-----"
      sleep 5
   else
      echo "    WARNING: Unable to find ${DAEMON} - will attempt to continue"
      let ERROR_COUNT=ERROR_COUNT+1
   fi
done

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
   IHFS=hd_ob83oax
   IHFS_SQL_FILE=/tmp/ihfs_data.sql
   if [ -f ${IHFS_SQL_FILE} ]; then
      echo "----Importing IHFS Data----"
      ${PG_BIN_DIR}/psql -q -U ${PG_USER} -p ${PG_PORT} -d ${IHFS} < ${IHFS_SQL_FILE} 
      echo "----IHFS Import Complete----"
   else
      echo "    WARNING: Unable to import IHFS Data - ${IHFS_SQL_FILE} not found, continuing"
      let ERROR_COUNT=ERROR_COUNT+1
   fi
else
   echo "    WARNING: Unable to verify ${COMPONENT} startup - will attempt to continue"
   let ERROR_COUNT=ERROR_COUNT+1
fi

echo "----Restoring Radar Server Configuration from backup---"
if [ -d ${RADAR_SERVER_BAK} -a -d ${RADAR_SERVER} ];then
   echo "    Copying config files from backup"
   su - awips -c "mv ${RADAR_SERVER}/data/config/drop-ins ${RADAR_SERVER}/data/config/drop-ins-bak"
   su - awips -c "cp -R ${RADAR_SERVER_BAK}/data/config/drop-ins ${RADAR_SERVER}/data/config/drop-ins"
   su - awips -c "mv ${RADAR_SERVER}/data/config/persist ${RADAR_SERVER}/data/config/persist-bak"
   su - awips -c "cp -R ${RADAR_SERVER_BAK}/data/config/persist ${RADAR_SERVER}/data/config/persist"
   echo "    Radar Server config restore complete"
else
   echo "   Unable to restore Radar Server config -- continuing"
fi

echo "**************************************************************************************"
echo "Finished Installing and Starting AWIPS DB on ${HOST}"
if [ ${ERROR_COUNT} -ne 0 ]; then
   echo "encountered ${ERROR_COUNT} issues in install - see log for details"
fi
echo "**************************************************************************************"
