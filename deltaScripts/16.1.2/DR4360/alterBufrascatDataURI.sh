#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from bufrascat

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=bufrascat

# table and constraint names from AScatObs.
echo "INFO: Start update of ${table} dataURI columns."

col=windSpd
echo "INFO: Update ${table}'s ${col}"

# Default value from BUFRPointDataAdapter.
${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}=-9999.0 where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

col=satId
echo "Info Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"
