#!/bin/bash

source settings.sh

# start PostgreSQL
/sbin/service edex_postgres start
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to start PostgreSQL."
   exit 1
fi
sleep 20

# get a list of the databases

# consider using the SQL query? ...
dbList=( $( psql --tuples-only -U awips -l | cut -f1 -d'|' | sed -e "s/^[[:space:]]*//g" -e "s/[[:space:]]*$//g" | grep -vE "template[01]|postgres|awips" ) )

# export the databases
for database in ${dbList[*]};
do
   echo "exporting database '${database}' ..."

   # determine if the database is postgis-enabled?
   # postgis-enabled PostgreSQL databases include one or both of the following tables:
   #      { spatial_ref_sys, geometry_columns }
   _SQL_="SELECT COUNT(*) FROM pg_catalog.pg_tables WHERE tablename IN ( 'spatial_ref_sys', 'geometry_columns' );"
   COUNT=`${PSQL} -U ${POSTGRESQL_USER} -p ${POSTGRESQL_PORT} -d ${database} -t -c "${_SQL_}"`
   
   # determine the suffix of the database export
   _export_suffix=".db"
   if [ ${COUNT} -ne 0 ]; then
      _export_suffix="${_export_suffix}.postgis"
   fi

   ${PG_DUMP} -Fc ${database} -U ${POSTGRESQL_USER} -p ${POSTGRESQL_PORT} > ${database}${_export_suffix}
   if [ $? -ne 0 ]; then
      exit 1
   fi
done

echo ""

# at this point, we know that the database export was successful; so, we will now be dropping
# every database that was previously identified.
for database in ${dbList[*]};
do
   echo "dropping database '${database}' ..."

   ${DROPDB} ${database} -U ${POSTGRESQL_USER} -p ${POSTGRESQL_PORT}
   if [ $? -ne 0 ]; then
      exit 1
   fi
done

# start PostgreSQL
/sbin/service edex_postgres stop
if [ $? -ne 0 ]; then
   echo "WARNING: Failed to stop PostgreSQL."
   exit 0
else
   sleep 20
fi

exit 0
