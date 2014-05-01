#!/bin/bash
#----------------------------------------------------------------------
# auto deploy script for a database server. Where possible, machine
# dependent values are obtained at runtime.
#
#----------------------------------------------------------------------
# Default values for these script variables may set using 'db.defaults'
RUN_FROM_DIR=/installer
PURGE_TIME=9

RCM_DAEMON=/etc/init.d/edex_rcm
#
#----------------------------------------------------------------------
# basic logic:
#   1) locate the PSQL installation
#   2) use PSQL to set purge time
# defaults:
#    default install works on awips-devdb
#
#----------------------------------------------------------------------
# args:
#
#----------------------------------------------------------------------
# limitations:
#   1) this script must be run as root
#   2) database and EDEX must be running
#
#----------------------------------------------------------------------

ERROR_COUNT=0
echo "----Starting Radar Server (using service start script)----"
if [ -e ${RCM_DAEMON} ];then
   echo "    running '${RCM_DAEMON} start'"
   ${RCM_DAEMON} start
   if [ $? -eq 0 ];then
      echo "    Radar Server started"
   else
      echo "    WARNING: Unable to start Radar Server, continuing"
      let ERROR_COUNT=ERROR_COUNT+1
   fi
else
   echo "    WARNING: Unable to start Radar Server, ${RCM_DAEMON} not found"
   let ERROR_COUNT=ERROR_COUNT+1
fi

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
echo "Running Set Purge Time Script on ${HOST} - ${DATE}"
echo "--------------------------------------------------------------------------------------"
ERROR_COUNT=0

# temporarily set retention time to '${PURGE_TIME}' hours to alleviate disk space issues
# get DB info from the DB machine
echo "----Determining DB connection information----"
POSTMASTER_STR=`ps ax -o user,args | grep postmaster | grep -v grep`

if [ -n "${POSTMASTER_STR}" ];then
   echo "----Setting purge to ${PURGE_TIME} hours---"
   PG_USER=`echo ${POSTMASTER_STR} | cut -d ' ' -f 1`
   PG_PORT=`echo ${POSTMASTER_STR} | cut -d ' ' -f 6`
   PG_DAEMON=`echo ${POSTMASTER_STR} | cut -d ' ' -f 2`
   PG_BIN_DIR=`dirname ${PG_DAEMON}`
   echo "    using PSQL in ${PG_BIN_DIR}"
   export PATH=${PG_BIN_DIR}:${PATH}
   PG_BASE_DIR=`dirname ${PG_BIN_DIR}`
   DB_BASE_DIR=`dirname ${PG_BASE_DIR}`
   echo "setting LD_LIBRARY_PATH = ${PG_BASE_DIR}/lib:${DB_BASE_DIR}/psql/lib"
   export LD_LIBRARY_PATH="${PG_BASE_DIR}/lib:${DB_BASE_DIR}/psql/lib"
   echo "${PG_BIN_DIR}/psql -q -U ${PG_USER} -p ${PG_PORT} -d metadata -c 'update awips.plugin_info set retentiontime = ${PURGE_TIME};'"
   ${PG_BIN_DIR}/psql -q -U ${PG_USER} -p ${PG_PORT} -d metadata -c "update awips.plugin_info set retentiontime = ${PURGE_TIME};"
   if [ $? -ne 0 ];then
      echo "    WARNING: Unable to set database purge time, continuing"
      let ERROR_COUNT=ERROR_COUNT+1
   fi
else
   echo "    WARNING: Unable to determine DB connection information; purge time not set" 
   let ERROR_COUNT=ERROR_COUNT+1
fi
echo "**************************************************************************************"
echo "Updating purge time in AWIPS DB on ${HOST}"
if [ ${ERROR_COUNT} -ne 0 ]; then
   echo "encountered ${ERROR_COUNT} issues in install - see log for details"
fi
echo "**************************************************************************************"
