#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from vaa

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=vaa

# table and constraint names from VAARecord.
echo "INFO: Start update of ${table} dataURI columns."
renameConstraint ${table} uk_${table}_datauri_fields

col=advisorynumber
echo "INFO: Update ${table}'s ${col}"
# Value from VAAParser.
${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}='null' where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"
