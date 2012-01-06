#!/bin/bash
# args:
#   $1 == (required) IHFS database name
#   $2 == (optional) location of IHFS dump file

if [ $# -eq 0 ]
then
   echo "usage database [dump file]"
   exit 0
fi
 
IHFS=${1}
if [ -n "${2}" ]
then
   IHFS_SQL_FILE=${2}
else
   IHFS_SQL_FILE=/tmp/ihfs_data.sql
fi

echo "restoring IHFS database dump"
echo "    restoring data from ${IHFS_SQL_FILE}"
echo "    Determining DB connection information"
POSTMASTER_STR=`ps ax -o user,args | grep postmaster | grep -v grep`
ERROR=0
if [ -n "${POSTMASTER_STR}" ];then
   echo "    setting PSQL running environment"
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
   echo "    ${PG_BIN_DIR}/psql -q -U ${PG_USER} -p ${PG_PORT} -d ${IHFS} < ${IHFS_SQL_FILE}"
   ${PG_BIN_DIR}/psql -q -U ${PG_USER} -p ${PG_PORT} -d ${IHFS} < ${IHFS_SQL_FILE}
   if [ $? -ne 0 ];then
      echo "    WARNING: Unable to import ${IHFS_SQL_FILE} to ${IHFS}"
      ERROR=1
   fi
else
   echo "    WARNING: Unable to determine DB connection information; data import not performed"
   ERROR=2 
fi

if [ $ERROR -eq 0 ]
then
   echo "${IHFS} data import completed successfully"
else
   echo "encountered errors importing ${IHFS}"
fi
exit ${ERROR} 
