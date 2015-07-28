#!/bin/bash
# DR #4360 - this update script will alter the dataURI column from tables derived from NPPSoundingRecord.

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh

# table and constraint names from CrimssRecord and NucapsRecord
tables=("nucaps" "crimss")

# columns from  NPPSoundingRecord.
cols=("latitude" "longitude")

echo "INFO: Start update of tables derived from NPPSoundingRecord."
for table in ${tables[@]} ; do
	if tableExists ${table}  ; then
		echo "INFO: Start update of ${table} dataURI columns."
		echo renameConstraint ${table} uk_${table}_datauri_fields

		for col in ${cols[@]} ; do
			echo "INFO: Update ${table}'s ${col}"
			echo ${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ;"
			echo updateNotNullCol ${table} ${col}
		done
		echo "INFO: ${table} dataURI columns updated successfully"
	else
		echo "WARNING: The table ${table} does not exist."
	fi
done

echo "INFO: Tables derived from NPPSoundingRecord updated."
