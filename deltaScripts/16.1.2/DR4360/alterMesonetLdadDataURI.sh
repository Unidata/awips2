#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from ldadmesonet

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=ldadmesonet

# table and constraint names form AScatObs.
echo "INFO: Start update of ${table} dataURI columns."
renameConstraint ${table} uk_${table}_datauri_fields

col=reportType
echo "INFO: Update ${table}'s ${col}"

${PSQL} -U awips -d metadata -c "DELETE FROM ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=dataProvider
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"
