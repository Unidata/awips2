#!/bin/bash
# DR #1846 - this update script will remove all datauri_idx indices from the metadata database

PSQL="/awips2/psql/bin/psql"
RETRIEVE_INDEX_SQL="SELECT indexname FROM pg_indexes WHERE indexname LIKE '%datauri_idx%' ORDER BY indexname;"
_index_list_txt=indexlist.txt

echo "INFO: update started"

# retrieve the indices
${PSQL} -U awips -d metadata -c "${RETRIEVE_INDEX_SQL}" -t -o ${_index_list_txt}
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to retrieve the list of data uri indices."
   echo "FATAL: The update has failed."
   exit 1
fi

for index in `cat ${_index_list_txt}`;
do
   # remove the index
   ${PSQL} -U awips -d metadata -c "DROP INDEX ${index};"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to drop index - ${index}."
      echo "FATAL: The update has failed."
      exit 1
   fi
done

rm -f ${_index_list_txt}

echo "INFO: the update has completed successfully!"

exit 0
