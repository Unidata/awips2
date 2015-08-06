#!/bin/bash 
# DR #4360 - this update script will alter the dataURI column from grid and grid_info

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
table=grid

# table and constraint names from GridRecord.
echo "INFO: Start update of ${table} dataURI columns."

col=info_id
echo "INFO: Update ${table}'s ${col}"
${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
updateNotNullCol ${table} ${col}

echo "INFO: ${table} dataURI columns updated successfully"

table=grid_info
strCols=("datasetid" "secondaryid" "ensembleid")
dropCols=("parameter_abbreviation" "level_id" "location_id")

# table and constraint names from GridInfoRecord.
echo "INFO: Start update of ${table} dataURI columns."
for col in ${strCols[@]} ; do
	echo "INFO: Update ${table}'s ${col}"
	${PSQL} -U awips -d metadata -c "UPDATE  ${table} SET ${col}='null' where ${col} is NULL ; "
	updateNotNullCol ${table} ${col}
done
for col in ${dropCols[@]} ; do
	echo "INFO: Update ${table}'s ${col}"
	${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
	updateNotNullCol ${table} ${col}
done


