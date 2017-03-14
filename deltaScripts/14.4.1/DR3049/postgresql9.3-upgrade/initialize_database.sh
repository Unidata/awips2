#!/bin/bash

source settings.sh

if [ -z "${AWIPS2_DATA_DIRECTORY}" ]; then
	echo "ERROR: AWIPS2_DATA_DIRECTORY must be set in settings.sh!"
	exit 1
fi

SQL_SHARE_DIR=${DATABASE_INSTALL}/sqlScripts/share/sql
SQL_MAPS_SHARE_DIR=${SQL_SHARE_DIR}/maps

# This Is The Log File That We Will Use To Log All SQL Interactions.
SQL_LOG=${SQL_SHARE_DIR}/sql_upgrade.log

# Add The PostgreSQL Libraries And The PSQL Libraries To LD_LIBRARY_PATH.
export LD_LIBRARY_PATH=${POSTGRESQL_INSTALL}/lib:$LD_LIBRARY_PATH
export LD_LIBRARY_PATH=${PSQL_INSTALL}/lib:$LD_LIBRARY_PATH

function create_sql_element()
{
   # $1 == element
   
   mkdir -p ${1}
   update_owner ${1}
}

function update_owner()
{
   # $1 == element
   chown ${POSTGRESQL_USER} ${1}
   chgrp ${AWIPS_DEFAULT_GROUP} ${1}
}

function init_db()
{   
   su ${POSTGRESQL_USER} -c \
      "${POSTGRESQL_INSTALL}/bin/initdb --auth=trust --locale=en_US.UTF-8 --pgdata=${AWIPS2_DATA_DIRECTORY} --lc-collate=en_US.UTF-8 --lc-ctype=en_US.UTF-8"
   
   if [ $? -ne 0 ]; then
      echo "init_db has failed!"
      exit 1
   fi
}

function control_pg_ctl()
{
   # $1 == pg_ctl command
   su ${POSTGRESQL_USER} -c \
      "${POSTGRESQL_INSTALL}/bin/pg_ctl ${1} -D ${AWIPS2_DATA_DIRECTORY} -o \"-p ${POSTGRESQL_PORT}\" -w"   
}

function execute_initial_sql_script()
{
   # Make The Necessary Replacements In The Script.
   perl -p -i -e "s/%{databaseUsername}/${POSTGRESQL_USER}/g" \
      ${1}
   echo ${AWIPS2_DATA_DIRECTORY} | sed 's/\//\\\//g' > .awips2_escape.tmp
   AWIPS2_DATA_DIRECTORY_ESCAPED=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   perl -p -i -e "s/%{database_files_home}/${AWIPS2_DATA_DIRECTORY_ESCAPED}/g" \
      ${1}

   # $1 == script to execute
   su ${POSTGRESQL_USER} -c \
      "${PSQL_INSTALL}/bin/psql -d postgres -U ${POSTGRESQL_USER} -q -p ${POSTGRESQL_PORT} -f ${1}" \
      > ${SQL_LOG} 2>&1
   if [ $? -ne 0 ]; then
      echo "Initial Database Setup has failed!"
      exit 1
   fi
}

function update_createDamcat()
{
   echo ${AWIPS2_DATA_DIRECTORY} | sed 's/\//\\\//g' > .awips2_escape.tmp
   AWIPS2_DATA_DIRECTORY_ESCAPED=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   perl -p -i -e "s/%{database_files_home}%/${AWIPS2_DATA_DIRECTORY_ESCAPED}/g" \
      ${SQL_SHARE_DIR}/createDamcat.sql
}

function createDamcatTablespace()
{
   local sql_script="damcat.sql"
   local current_location=`pwd`

   touch ${sql_script}

   echo "DROP TABLESPACE IF EXISTS damcat;" > ${sql_script}
   echo "CREATE TABLESPACE damcat OWNER awips LOCATION '/awips2/data/damcat';" >> ${sql_script}
   echo "COMMENT ON TABLESPACE damcat IS 'DAMCAT Database tablespace';" >> ${sql_script}

   execute_psql_sql_script ${current_location}/${sql_script} postgres

   rm -f ${current_location}/${sql_script}
}

function update_createEbxml()
{
   echo ${AWIPS2_DATA_DIRECTORY} | sed 's/\//\\\//g' > .awips2_escape.tmp
   AWIPS2_DATA_DIRECTORY_ESCAPED=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   perl -p -i -e "s/%{database_files_home}%/${AWIPS2_DATA_DIRECTORY_ESCAPED}/g" \
      ${SQL_SHARE_DIR}/createEbxml.sql
}

function update_createHMDB()
{
   echo ${AWIPS2_DATA_DIRECTORY} | sed 's/\//\\\//g' > .awips2_escape.tmp
   AWIPS2_DATA_DIRECTORY_ESCAPED=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   perl -p -i -e "s/%{database_files_home}%/${AWIPS2_DATA_DIRECTORY_ESCAPED}/g" \
      ${SQL_SHARE_DIR}/createHMDB.sql
}

function update_createMaps()
{
   echo ${AWIPS2_DATA_DIRECTORY} | sed 's/\//\\\//g' > .awips2_escape.tmp
   AWIPS2_DATA_DIRECTORY_ESCAPED=`cat .awips2_escape.tmp`
   rm -f .awips2_escape.tmp
   # Update the sql script that creates the maps database / tables.
   perl -p -i -e "s/%{database_files_home}%/${AWIPS2_DATA_DIRECTORY_ESCAPED}/g" \
      ${SQL_MAPS_SHARE_DIR}/createMapsDb.sql
}

function execute_psql_sql_script()
{
   # $1 == script to execute
   # $2 == database
   
   su ${POSTGRESQL_USER} -c \
      "${PSQL_INSTALL}/bin/psql -d ${2} -U ${POSTGRESQL_USER} -q -p ${POSTGRESQL_PORT} -f \"${1}\"" \
      >> ${SQL_LOG} 2>&1
   if [ $? -ne 0 ]; then
      echo "Failed to execute SQL script: ${1}!"
      exit 1
   fi
}

#temporarily relocate the PostgreSQL configuration
_BACKUP_CONF=/awips2/postgresql-configuration-bak
if [ -d ${_BACKUP_CONF} ]; then
   rm -rf ${_BACKUP_CONF}
   if [ $? -ne 0 ]; then
      exit 1
   fi
fi
mkdir -p ${_BACKUP_CONF}
if [ $? -ne 0 ]; then
   exit 1
fi

POSTGRESQL_CONFIGURATION=( 'pg_hba.conf' 'pg_ident.conf' 'postgresql.conf' )
for configuration in ${POSTGRESQL_CONFIGURATION[*]};
do
   mv ${AWIPS2_DATA_DIRECTORY}/${configuration} \
      ${_BACKUP_CONF}/${configuration}
   if [ $? -ne 0 ]; then
      exit 0
   fi
done

# purge the existing data directory
rm -rf ${AWIPS2_DATA_DIRECTORY}/*
if [ $? -ne 0 ]; then
	exit 1
fi 

init_db

control_pg_ctl "start"
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to start PostgreSQL."
   exit 1
fi
sleep 20

create_sql_element ${METADATA}
create_sql_element ${IFHS}
create_sql_element ${DAMCAT}
create_sql_element ${HMDB}
create_sql_element ${EBXML}
create_sql_element ${MAPS}

execute_initial_sql_script ${SQL_SHARE_DIR}/initial_setup_server.sql

execute_psql_sql_script ${SQL_SHARE_DIR}/fxatext.sql metadata

createDamcatTablespace

update_createHMDB
execute_psql_sql_script ${SQL_SHARE_DIR}/createHMDB.sql postgres
   
update_createEbxml
execute_psql_sql_script ${SQL_SHARE_DIR}/createEbxml.sql postgres

control_pg_ctl "stop"
if [ $? -ne 0 ]; then
   echo "WARNING: Failed to stop PostgreSQL!"
else
   sleep 20
fi

# restore the PostgreSQL configuration
for configuration in ${POSTGRESQL_CONFIGURATION[*]};
do
   mv ${_BACKUP_CONF}/${configuration} \
      ${AWIPS2_DATA_DIRECTORY}/${configuration}
   if [ $? -ne 0 ]; then
      exit 1
   fi
done

rm -rf ${_BACKUP_CONF}
if [ $? -ne 0 ]; then
   echo "WARNING: Failed to Remove - ${_BACKUP_CONF}!"
fi

exit 0
