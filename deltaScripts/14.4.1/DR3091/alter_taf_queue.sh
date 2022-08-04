#!/bin/bash
# DR #3091 This changes the awips.taf_queue forecasterid column from type integer
#          to a varchar converting any values in the table.

PSQL="/awips2/psql/bin/psql"

echo "INFO: Altering awips.taf_queue's forecaster column."

${PSQL} -U awips -d metadata -c "alter table awips.taf_queue alter column forecasterid set data type varchar(255)
 using to_char(forecasterid, '000');"

if [ $? -ne 0 ]; then
   echo "ERROR: Failed to alter tabelawips.taf_queue's column forecasterid."
   echo "FATAL: The update has failed."
   exit 1
fi

echo "INFO: awips.taf_queue's forecaster column altered successfully."
