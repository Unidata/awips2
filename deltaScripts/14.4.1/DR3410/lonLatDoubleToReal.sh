#!/bin/bash
# DR #3410 - this update script will change columns from Double to Real

# operate on tables that were built with classes that embed SurfaceObsLocation or AircraftObsLocation
# only these tables contain the column 'locationdefined'
TABLES=$(psql -U awips -d metadata -tc "select table_name from information_schema.columns where column_name = 'locationdefined'")
COLUMNS=(latitude longitude)
PSQL="/awips2/psql/bin/psql"

# takes two args: a table name and a column name
# alters the column in the table to real
function changeToReal {
   echo "INFO: Changing table $1 column $2 to real."
   ${PSQL} -U awips -d metadata -c "ALTER TABLE $1 ALTER COLUMN $2 TYPE real;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to change the column $2 for table $1"
      return 1
   fi
}

msg="INFO: All columns changed successfully"

for table in ${TABLES[*]}
do
  echo "INFO: Altering table $table."
  for column in ${COLUMNS[*]}
  do
    changeToReal $table $column || msg="INFO: Operation completed, some columns could not be changed"
  done
done

echo $msg
