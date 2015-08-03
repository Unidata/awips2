#!/bin/bash 
# DR #4360 - this update script will alter the dataURI column from tables with embedded AircraftObsLocation.

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh

embClass="AircraftObsLocation"

# Tables with embedded AircraftObsLocation.
tables=("acars")

# Col names from AircraftObsLocation.
cols=("flightlevel" "stationid" "latitude" "longitude")

for table in ${tables[@]} ; do
	echo "INFO: Start update of embedded ${embClass} dataURI columns in table ${table}."

	for col in ${cols[@]} ; do
		echo "INFO: Update ${table}' ${col}"
		${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
		updateNotNullCol ${table} ${col}
	done
	echo "INFO: Finish update of embedded ${embClass} dataURI columns in table ${table}."
done

tables=("airep" "pirep")

#tables have the same non-nullable @DataURI columns
cols=("reporttype" "corindicator")

# table and constraint names from AirepRecord and PirepRecord.
for table in ${tables[@]} ; do
	if tableExists $table ; then
		echo "INFO: Start update of ${table} dataURI columns."

		for col in ${cols[@]} ; do
					echo "INFO: Update ${table}'s ${col}"
			${PSQL} -U awips -d metadata -c "UPDATE  ${table} SET ${col}='null' where ${col} is NULL ; "
			updateNotNullCol ${table} ${col}
		done

		echo "INFO: ${table} dataURI columns updated successfully"
	else
		echo "WARNING: ${table} does not exist."
	fi
done

