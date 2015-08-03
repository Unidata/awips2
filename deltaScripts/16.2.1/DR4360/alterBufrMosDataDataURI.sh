#!/bin/bash
# DR #4360 - this update script will alter the dataURI columns for tables derived from BufrMosData.
# It also updates the bufrmos_location table reference by these tables.

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh


table=bufrmos_location
cols=("stationId" "latitude" "longitude")

# table and constraint names from BufrMosDataLocation.
echo "INFO: Start update of ${table} dataURI columns."

for col in ${cols[@]} ; do
	echo "INFO: Update ${table}'s ${col}"
	${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
	updateNotNullCol ${table} ${col}
done
echo "INFO: ${table} dataURI columns updated successfully"

# Tables derived from BufrMostData unique constraints hanelded in alterUniqueConstraintNames.sh
tables=("bufrmosavn" "bufrmoseta" "bufrmosgfs" "bufrmoshpc" "bufrmoslamp" "bufrmosmrf" "bufrmosngm")

echo "INFO: Updating bufrmos tables"
col=location_id
for table in ${tables[@]} ; do
	${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
	updateNotNullCol ${table} ${col}
done
echo "INFO: Done updateing bufrmos tables"
