#!/bin/bash

# dumps the subscription tables for restoration...
# created for ticket #3755 - mwf
#
# Usage:
#   dump-subscription.sh result-file [user] [db-name] [schemata] [host-name] [port]
# Args:
#   $1 :: path/name of result file
#   $2 :: (optional) user to perform the save
#   $3 :: (optional) database name
#   $4 :: (optional) schemata
#   $5 :: (optional) db host name -- default is 'localhost'
#   $6 :: (optional) port -- default is '5432'
# Assumptions:
#   Assumes that PostgreSQL is running -- used to locate postgres bin
#
# tables to dump -- modify if needed
TABLES_TO_DUMP='subscriptions '\
'replacements'
#
TEMP_FILE=/tmp/temp.sql
DAEMON=pg_dump
# validate required argument -- first arg required
if [ $# -lt 1 ];then
   echo "one or more required arguments missing -- exiting"
   echo ""
   echo "usage:"
   echo "  dump-subscription.sh out-file [user] [db-name] [schemata] [host-name] [port]"
   echo ""
   exit 1
fi
# read command line arguments
OUT_FILE=${1}
USER=${2}
DATABASE=${3}
SCHEMATA=${4}
HOST=${5}
PORT=${6}

# set defaults if needed
if [ -z "${USER}" ];then
   USER="awips"
fi
if [ -z "${DATABASE}" ];then
   DATABASE="metadata"
fi
if [ -z "${SCHEMATA}" ];then
   SCHEMATA="subscription"
fi
if [ -z "${HOST}" ];then
   HOST="localhost"
fi
if [ -z "${PORT}" ];then
   PORT=5432
fi

# delete previous dump
if [ -f ${OUT_FILE} ]; then
   rm -f ${OUT_FILE}
fi

# get the location of the postgres bin directory -- will try to locate pg_dump
# directly, if that fails, will work from running postmaster.
PG_DAEMON=`which pg_dump`
if [ -z "${PG_DAEMON}" ]; then
   # did not find pg_dump in path, will attempt to locate postmaster
   POSTMASTER_STR=`ps ax -o user,args | grep postmaster | grep -v grep`
   if [ -z "${POSTMASTER_STR}" ];then
      echo "unable to find PostgreSQL bin directory -- exiting"
      exit 1
   else
      PG_DAEMON=`echo ${POSTMASTER_STR} | cut -d ' ' -f 2`
   fi
fi
PG_BIN_DIR=`dirname ${PG_DAEMON}`
echo "using pg_dump in ${PG_BIN_DIR}" 
PG_BASE_DIR=`dirname ${PG_BIN_DIR}`
DB_BASE_DIR=`dirname ${PG_BASE_DIR}`
echo "setting LD_LIBRARY_PATH = ${PG_BASE_DIR}/lib:${DB_BASE_DIR}/psql/lib"
export LD_LIBRARY_PATH="${PG_BASE_DIR}/lib:${DB_BASE_DIR}/psql/lib"

for table in ${TABLES_TO_DUMP} ;do
   echo "exporting DDL for '${SCHEMATA}.${table}'"
   if [ -z "${HOST}" ];then
      ${PG_BIN_DIR}/${DAEMON} -a -U ${USER} -D ${DATABASE} -p ${PORT} -t "${SCHEMATA}.${table}" > ${TEMP_FILE}
   else
      ${PG_BIN_DIR}/${DAEMON} -a -U ${USER} -D ${DATABASE} -h ${HOST} -p ${PORT} -t "${SCHEMATA}.${table}" > ${TEMP_FILE}
   fi
   COUNT=`grep -c INSERT ${TEMP_FILE}`
   if [ ${COUNT} -eq 0 ];then
      echo "   no data exported from '${SCHEMATA}.${table}'"
   else
      echo "   exported ${COUNT} rows from '${SCHEMATA}.${table}'"
      cat ${TEMP_FILE} >> ${OUT_FILE}
   fi
done 
