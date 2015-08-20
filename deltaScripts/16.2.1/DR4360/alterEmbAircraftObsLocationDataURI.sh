#!/bin/bash 
# DR #4360 - this update script will alter the dataURI column from tables with embedded AircraftObsLocation.

PSQL="/awips2/psql/bin/psql"

cmdDir=`dirname $0`

source ${cmdDir}/commonFunctions.sh

# Embedded column names from AircraftObsLocation.
embCols=("flightlevel" "stationid" "latitude" "longitude")
embClass="AircraftObsLocation"

# Tables with embedded AircraftObsLocation.
tables=("acars")
cols=tailnumber

for table in ${tables[@]} ; do
		echo "INFO: Start update of ${table} dataURI columns."
		echo "INFO: update embedded ${embClass} columns in table ${table}."
	for col in ${embCols[@]}  ; do
		echo "INFO: Update ${table}'s column ${col}"
				${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
		updateNotNullCol ${table} ${col}
	done
	echo "INFO: Update ${table}'s other non-nullable columns." 
	for col in ${cols[@]} ; do
		echo "INFO: Update ${table}'s column ${col}"
		${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
		updateNotNullCol ${table} ${col}
	done
	echo "INFO: Finish update dataURI columns in table ${table}."
done

tables=("airep" "pirep")
#tables have the same non-nullable @DataURI columns
cols=("reporttype" "corindicator")

# table and constraint names from AirepRecord and PirepRecord.
for table in ${tables[@]} ; do
	if tableExists $table ; then
		echo "INFO: Start update of ${table} dataURI columns."
		echo "INFO: update embedded ${embClass} columns in table ${table}."
		for col in ${embCols[@]}  ; do
			echo "INFO: Update ${table}'s column ${col}"
			${PSQL} -U awips -d metadata -c "DELETE from ${table} where ${col} is NULL ; "
			updateNotNullCol ${table} ${col}
		done
		echo "INFO: Update ${table}'s other non-nullable columns." 

		for col in ${cols[@]} ; do
					echo "INFO: Update ${table}'s ${col}"
			${PSQL} -U awips -d metadata -c "UPDATE  ${table} SET ${col}='' where ${col} is NULL ; "
			updateNotNullCol ${table} ${col}
		done
		echo "INFO: Finish update dataURI columns in table ${table}."
	else
		echo "WARNING: ${table} does not exist."
	fi
done

