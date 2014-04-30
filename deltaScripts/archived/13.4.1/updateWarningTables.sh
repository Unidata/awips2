#!/bin/bash
# Main script for updating warning database structure

PSQL="/awips2/psql/bin/psql"
PYTHON="/awips2/python/bin/python"

SQL_SCRIPT="alterWarningTables.sql"

# ensure that the sql script is present
if [ ! -f ${SQL_SCRIPT} ]; then
   echo "ERROR: the required sql script - ${SQL_SCRIPT} was not found."
   echo "FATAL: the update has failed!"
   exit 1
fi


echo "Adding ugczones column to warning tables"
${PSQL} -U awips -d metadata -f ${SQL_SCRIPT}
if [ $? -ne 0 ]; then
   echo "FATAL: the update has failed!"
   exit 1
fi

TABLES="practicewarning warning"
for table in $TABLES
do
   echo 
   echo "Querying for $table ugc zones"
   RETRIEVE_UGC_ZONES_SQL="SELECT parentwarning, zone FROM warning_ugczone where parentwarning in (select id from $table) order by parentwarning, key"
   _ugc_zone_txt="${table}UgcZones.txt"

   ${PSQL} -U awips -d metadata -c "${RETRIEVE_UGC_ZONES_SQL}" -t -o ${_ugc_zone_txt}
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to retrieve the ugc zones for $table table."
      echo "FATAL: The update has failed."
      exit 1
   fi

   echo 
   echo "Parsing ugc zones for insertion into $table table"
   PYTHON_PARSE_SCRIPT="parseUgcZones.py"
   if [ ! -f ${PYTHON_PARSE_SCRIPT} ]; then
      echo "ERROR: the required python script - ${PYTHON_PARSE_SCRIPT} was not found."
      echo "FATAL: the update has failed!"
      exit 1
   fi

   ${PYTHON} ${PYTHON_PARSE_SCRIPT} ${table} ${_ugc_zone_txt}
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to parse ugc zones."
      echo "FATAL: The update has failed."
      exit 1
   fi

   echo 
   echo "Adding ugc zones to $table table"
   # ${table}UgcZonesUpdates.sql generated from parseParmIds.py
   ${PSQL} -U awips -d metadata -q -f ${table}UgcZonesUpdates.sql
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to add ugc zones."
      echo "FATAL: The update has failed."
      exit 1
   fi
done

#remove warning_ugczone
echo 
echo "Dropping warning_ugczone table"
DROP_TABLE_SQL="DROP TABLE warning_ugczone"

${PSQL} -U awips -d metadata -c "${DROP_TABLE_SQL}"

echo 
echo "Running full vacuum for warning"
${PSQL} -U awips -d metadata -c "VACUUM FULL VERBOSE ANALYZE warning"
