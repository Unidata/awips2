#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from ccfp

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=ccfp

# table and constraint names from CcfpRecord.
echo "INFO: Start update of ${table} dataURI columns."

col=producttype
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"
