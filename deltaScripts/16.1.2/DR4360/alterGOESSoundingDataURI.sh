#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from goessounding

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=goessounding

# table and constraint names form AScatObs.
echo "INFO: Start update of ${table} dataURI columns."
renameConstraint ${table} uk_${table}_datauri_fields
echo "INFO: ${table} dataURI columns updated successfully"
