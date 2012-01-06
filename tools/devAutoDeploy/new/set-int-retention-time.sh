#!/bin/bash
# args:
# $1 == (optional) new purge time

if [ -n "${1} ]
then
   PURGE_TIME=${1}
else
   PURGE_TIME=48
fi

DEF_PURGE_TIME=24
echo "resetting purge times from ${DEF_PURGE_TIME} to ${PURGE_TIME}"
echo "    Determining DB connection information"
POSTMASTER_STR=`ps ax -o user,args | grep postmaster | grep -v grep`
ERROR=0
if [ -n "${POSTMASTER_STR}" ];then
   echo "    Setting purge to ${PURGE_TIME} hours"
   PG_USER=`echo ${POSTMASTER_STR} | cut -d ' ' -f 1`
   PG_PORT=`echo ${POSTMASTER_STR} | cut -d ' ' -f 6`
   PG_DAEMON=`echo ${POSTMASTER_STR} | cut -d ' ' -f 2`
   PG_BIN_DIR=`dirname ${PG_DAEMON}`
   echo "    using PSQL in ${PG_BIN_DIR}"
   export PATH=${PG_BIN_DIR}:${PATH}
   PG_BASE_DIR=`dirname ${PG_BIN_DIR}`
   DB_BASE_DIR=`dirname ${PG_BASE_DIR}`
   echo "    setting LD_LIBRARY_PATH = ${PG_BASE_DIR}/lib:${DB_BASE_DIR}/psql/lib"
   export LD_LIBRARY_PATH="${PG_BASE_DIR}/lib:${DB_BASE_DIR}/psql/lib"
   echo "    ${PG_BIN_DIR}/psql -q -U ${PG_USER} -p ${PG_PORT} -d metadata -c 'update awips.plugin_info set retentiontime = ${PURGE_TIME} where retentiontime = ${DEF_PURGE_TIME};'"
   ${PG_BIN_DIR}/psql -q -U ${PG_USER} -p ${PG_PORT} -d metadata -c "update awips.plugin_info set retentiontime = ${PURGE_TIME} where retentiontime = ${DEF_PURGE_TIME};"
   if [ $? -ne 0 ];then
      echo "    WARNING: Unable to set database purge time, continuing"
      ERROR=1
   fi
else
   echo "    WARNING: Unable to determine DB connection information; purge time not set"
   ERROR=2 
fi

if [ $ERROR -eq 0 ]
then
   echo "purge time reset completed successfully"
else
   echo "encountered errors resetting purge time"
fi
exit ${ERROR} 
