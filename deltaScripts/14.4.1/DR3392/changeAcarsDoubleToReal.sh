#!/bin/bash
# DR #3392 - this update script will change columns from Double to Real

TABLES=(acars acarssoundinglayer)
COLUMNS=(dwpt humidity mixingratio pressure temp windspeed)
PSQL="/awips2/psql/bin/psql"

# takes two args: a table name and a column name
# alters the column in the table to real
function changeToReal {
   echo "INFO: Changing table $1 column $2 to real."
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 ALTER COLUMN $2 TYPE real;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to change the column $2 for table $1"
      echo "FATAL: The update has failed."
      exit 1
   fi
}


for table in ${TABLES[*]}
do
  echo "INFO: Altering table $table."
  for column in ${COLUMNS[*]}
  do
    changeToReal $table $column
  done
done

echo "INFO: All columns changed successfully"
