#!/bin/bash
# DR #2581 - This update script will add the dataURI column to the lsr table
#            The column needs to be there for 14.2.1.
#            Only run this if you have already run dropLSRdataURI.sh, otherwise
#             don't run this script or that one.


PSQL="/awips2/psql/bin/psql"

# adds the datauri constraint and column
function addDataUriLSR {
   echo "INFO: Adding DataURI column to lsr"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE lsr DROP CONSTRAINT IF EXISTS lsr_latitude_longitude_stationId_reftime_forecasttime_eventtype;"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE lsr ADD COLUMN datauri varchar(255);"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE lsr ADD CONSTRAINT lsr_datauri_key UNIQUE (datauri);"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to repair dataURI for lsr"
      echo "FATAL: The update has failed."
      exit 1
   fi
}



echo "INFO: Adding LSR dataURI column back in."

addDataUriLSR

echo "INFO: LSR dataURI column added successfully"
