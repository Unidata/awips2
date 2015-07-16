#!/bin/bash 
# DR #4360 - this update script will alter the dataURI column from acars

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=acars

# table and constraint names form ACARSRecord.
echo "INFO: Start update of ${table} dataURI columns."
renameConstraint ${table} uk_${table}_datauri_fields

col=tailNumber
echo "INFO: Update ${table}' ${col}"
${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"
