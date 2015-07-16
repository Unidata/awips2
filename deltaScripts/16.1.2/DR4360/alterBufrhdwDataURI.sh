#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from bufrhdw

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=bufrhdw

# table and constraint names form AScatObs.
echo "INFO: Start update of ${table} dataURI columns."
renameConstraint ${table} uk_${table}_datauri_fields

col=pressure
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}=-9999 where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=satType
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}='Null' where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"
