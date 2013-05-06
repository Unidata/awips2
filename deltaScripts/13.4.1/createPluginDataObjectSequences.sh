#!/bin/bash
# DR #1857 - this update script will create sequences for the metadata database.
# No arguments are passed to this script.  It reads sequences.txt from the local directory.

PSQL="/awips2/psql/bin/psql"
_sequences_txt=sequences.txt

echo "INFO: Creating sequences."

for sequence in `cat ${_sequences_txt}`;
do
   table=${sequence%%seq}
   echo "INFO: Creating sequence ${sequence} for table ${table}"
   ##To start sequence with (current max id + 1 ), uncomment the next two lines.
   #sequenceStart=`${PSQL} -tU awips -d metadata -c "SELECT max(id) FROM ${table};" | tr -d '\n' | tr -d ' '`
   #let sequenceStart=${sequenceStart}+1
   ${PSQL} -U awips -d metadata -c "CREATE SEQUENCE ${sequence} INCREMENT 1 MINVALUE 1 MAXVALUE 9223372036854775807 CACHE 1 START ${sequenceStart:=1};"
   ${PSQL} -U awips -d metadata -c "ALTER TABLE ${sequence} OWNER TO awips;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to create sequence ${sequence}."
      echo "FATAL: The update has failed."
      exit 1
   fi
done

echo "INFO: sequence creation has completed successfully!"

exit 0
