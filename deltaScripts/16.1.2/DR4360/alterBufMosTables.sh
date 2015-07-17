#!/bin/bash 
# DR #4360 - this update script will alter the tables derived from BufrMosData.

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh

#tables derived from BufrMosData.
tables=("bufrmosavn" "bufrmoseta" "bufrmosgfs" "bufrmoshpc" "bufrmoslamp" "bufrmosmrf" "bufrmosngm")

# table and constraint name BufrMosAvnData.
echo "INFO: Start update of BufrMosData derived tables"

for  table in ${tables[@]} ; do
	renameConstraint ${table} uk_${table}_datauri_fields
	echo "INFO updated ${table}."
done

echo "INFO: Finish update of BufrMosData derived tables"
