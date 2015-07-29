#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from madis

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=madis

# table and constraint names from MadisRecord.
echo "INFO: Start update of ${table} dataURI columns."
col=provider
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=subProvider
echo "Info Update ${table}'s ${col}"
# When subProvider missing MadisDecoder already enters the string "null"; therefore should never have a NULL entry.
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=restriction
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"
