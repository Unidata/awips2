#!/bin/bash 
# DR #4360 - this update script will alter all tables modified under this DR
# This assumes there is a script starting with alter that does the work for a given table. 

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

cd ${cmdDir}
echo "INFO Update tables"
for script in alter*.sh ; do
	./$script
done

echo "INFO: Tables updated successfully"
