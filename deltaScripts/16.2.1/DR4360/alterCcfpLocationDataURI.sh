#!/bin/bash 
# DR #4360 - this update script will alter tables with the embedded dataURI columns from CcfpLocation

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh
tables=("ccfp")
embedded=CcfpLocation

# DataURI cols from CcfpRecord.
cols=("boxlat" "boxlong")

echo "INFO: Start update of tables with embedded CcfpLocation"

for  table in ${tables[@]} ; do
	echo "INFO: update ${embedded} columns  ${cols[@]} for table ${table}"
	for col in ${cols[@]} ; do
		${PSQL} -U awips -d metadata -c "DELETE from  ${table} where ${col} is NULL ; "
		updateNotNullCol ${table} ${col}
	done
done

echo "INFO: Tables with embedded CcfpLocation updated successfully"
