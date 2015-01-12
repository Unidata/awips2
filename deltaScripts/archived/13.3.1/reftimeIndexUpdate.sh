#!/bin/bash
# DR #1846 - this DR will remove all reftimeindex indices from the metadata database.

PSQL="/awips2/psql/bin/psql"

#_timeindexname="timeindex"
RETRIEVE_INDEX_SQL="SELECT indexname FROM pg_indexes WHERE indexname LIKE '%reftimeindex%' ORDER BY indexname;"
_index_list_txt=indexlist.txt
#_reftime_indx_length=12

echo "INFO: update started"

# retrieve the reftime indices
${PSQL} -U awips -d metadata -c "${RETRIEVE_INDEX_SQL}" -t -o ${_index_list_txt}
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to retrieve the list of data uri indices."
   echo "FATAL: The update has failed."
   exit 1
fi

for index in `cat ${_index_list_txt}`;
do
   # determine which table the index is in.
   SQL="SELECT tablename FROM pg_indexes WHERE indexname = '${index}';"
   table=`${PSQL} -U awips -d metadata -c "${SQL}" -t`
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to determine which table ${index} belongs to."
      echo "FATAL: The update has failed."
      exit 1
   fi

   #length=${#index}
   #let LENGTH_DIFF=${length}-${_reftime_indx_length}
   #prefix=${index:0:$LENGTH_DIFF}

   # remove the index
   ${PSQL} -U awips -d metadata -c "DROP INDEX ${index};"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to drop index - ${index}."
      echo "FATAL: The update has failed."
      exit 1
   fi

   # create the new index
   SQL="CREATE INDEX ${index} ON ${table} USING btree(reftime, forecasttime);"
   ${PSQL} -U awips -d metadata -c "${SQL}"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to create index ${table}${_timeindexname} on table ${table}."
      echo "FATAL: The update has failed."
      exit 1
   fi
done

rm -f ${_index_list_txt}

RETRIEVE_INDEX_SQL="SELECT indexname FROM pg_indexes WHERE indexname LIKE '%fcsttimeindex%' ORDER BY indexname;"

# retrieve the fcsttime indices
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
