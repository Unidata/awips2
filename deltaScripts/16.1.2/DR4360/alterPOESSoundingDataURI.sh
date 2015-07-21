#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from poessounding

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=poessounding

# table and constraint names from POESSounding.
echo "INFO: Start update of ${table} dataURI columns."
renameConstraint ${table} uk_${table}_datauri_fields

echo "INFO: ${table} dataURI columns updated successfully"
