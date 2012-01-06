#!/bin/bash
#----------------------------------------------------------------------
# Auto script for a database server.
#
#----------------------------------------------------------------------


HOST=`echo $HOSTNAME | cut -d. -f1`

#
#----------------------------------------------------------------------
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

echo "--------------------------------------------------------------------------------------"
echo "Starting EDEX/Database on ${HOST} - ${DATE}"
echo "--------------------------------------------------------------------------------------"

INSTALL_DIR="/awips2"
yum clean all
export http_proxy=
yum groupinstall 'AWIPS II Database Server' -y

chown -R awips:fxalpha /awips2/edex/data/*
chmod -Rf 775 /awips2/edex/data/*

service httpd-pypies start

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

echo "**************************************************************************************"
echo "Finished Installing and Starting AWIPS DB on ${HOST}"
if [ ${ERROR_COUNT} -ne 0 ]; then
   echo "encountered ${ERROR_COUNT} issues in install - see log for details"
fi
echo "**************************************************************************************"

