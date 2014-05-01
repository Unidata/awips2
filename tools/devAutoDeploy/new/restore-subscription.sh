#!/bin/bash
# Restores previously dumped subscription 
#
# Usage:
#   restore-subscription.sh result-file [user] [db-name] [schemata] [host-name] [port]
# Args:
#   $1 :: path/name of result file
#   $2 :: (optional) user to perform the save
#   $3 :: (optional) database name
#   $4 :: (optional) db host name -- default is 'localhost'
#   $5 :: (optional) port -- default is '5432'
# Assumptions:
#   Assumes that PostgreSQL is running -- used to locate postgres bin

# validate required argument -- first arg required
if [ $# -lt 1 ];then
   echo "one or more required arguments missing -- exiting"
   echo ""
   echo "usage:"
   echo "  restore-subscription.sh out-file [user] [db-name] [schemata] [host-name] [port]"
   echo ""
   exit 1
fi

# read command line arguments
DDL_FILE=${1}
USER=${2}
DATABASE=${3}
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

echo "restoring Subscriptions database dump"
echo "   restoring data from ${DDL_FLE}"

PG_DAEMON=psql
PG_PATH=`which ${PG_DAEMON}`
# need to get the location of the psql app -- if unavailable, exit
if [ -z "${PG_DAEMON}" ]; then
   echo "unable to find ${PG_DAEMON} -- exiting"
   exit 1
fi
echo "determining DB connection information"
PG_BIN_DIR=`dirname ${PG_PATH}`
echo "    using ${PG_DAEMON} in ${PG_BIN_DIR}" 
PG_BASE_DIR=`dirname ${PG_BIN_DIR}`
echo "    setting LD_LIBRARY_PATH = ${PG_BASE_DIR}/lib"
export LD_LIBRARY_PATH="${PG_BASE_DIR}/lib"
export PATH=${PG_BIN_DIR}:${PATH}
echo "attempting data restore"
echo "${PG_PATH} -q -U ${USER} -d ${DATABASE} -h ${HOST} -p ${PORT} < ${DDL_FILE}"
${PG_PATH} -q -U ${USER} -d ${DATABASE} -h ${HOST} -p ${PORT}  < ${DDL_FILE}
if [ $? -ne 0 ];then
   echo "    WARNING: Unable to import ${DDL_FLE} to ${DATABASE}"
else
   echo "    ${DATABASE} data import completed successfully"
fi

