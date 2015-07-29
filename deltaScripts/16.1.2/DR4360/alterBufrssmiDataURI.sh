#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns from bufrssmi

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=bufrssmi

# table and constraint names form BufrMTHDWObs.
echo "INFO: Start update of ${table} dataURI columns."

col=satid
echo "INFO: Update ${table}'s ${col}"
# The IDecoderConstants.VAL_MISSING used by SSMIDataAdapter.
${PSQL} -U awips -d metadata -c "UPDATE ${table} SET ${col}=-9999998 where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"
