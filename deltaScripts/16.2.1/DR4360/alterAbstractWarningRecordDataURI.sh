#!/bin/bash 
# DR #4360 - this update script will alter the dataURI columns for tables derived from AbstractWarningRecord

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
tables=("practicewarning" "warning")
nullCols=("wmoid" "pil" "xxxid" "act" "etn" "phensig")

# table and constraint names from AbstractWarningRecord.

for table in ${tables[@]} ; do
	echo "INFO: Start update of ${table} dataURI columns."

	for col in ${nullCols[@]} ; do
		echo "INFO: Update ${table}' ${col}"
		${PSQL} -U awips -d metadata -c "UPDATE  ${table} SET ${col}='' where ${col} is NULL ; "
		updateNotNullCol ${table} ${col}
	done

	col=seg
	${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
	updateNotNullCol ${table} ${col}

	echo "INFO: ${table} dataURI columns updated successfully"
done