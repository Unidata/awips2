#!/bin/bash

#!/bin/bash
# DR #1846 - this update script will remove all hdffileid columns from the metadata database

PSQL="/awips2/psql/bin/psql"
SQL="SELECT table_name FROM information_schema.columns WHERE column_name = 'hdffileid';"
_table_list_txt=tablelist.txt

echo "INFO: update started."

# retrieve the tables

${PSQL} -U awips -d metadata -c "${SQL}" -t -o ${_table_list_txt}
if [ $? -ne 0 ]; then
   echo "ERROR: Failed to retrieve the list of tables."
   echo "FATAL: The update has failed."
   exit 1
fi

for table in `cat ${_table_list_txt}`;
do
   ${PSQL} -U awips -d metadata -c "ALTER TABLE ${table} DROP COLUMN hdffileid CASCADE;"
   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to drop column hdffileid in table ${table}."
      echo "FATAL: The update has failed."
      exit 1
   fi
done

rm -f ${_table_list_txt}

echo "INFO: the update has completed successfully!"

exit 0
