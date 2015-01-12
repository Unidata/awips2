#!/bin/bash
# run the update
/awips2/psql/bin/psql -U awips -d metadata -f CreateNewGfeTables.sql
if [ $? -ne 0 ]; then
   echo "FATAL: the update has failed!"
   exit 1
fi

echo "INFO: the update has completed successfully!"

exit 0
