#!/bin/bash
# run the update
/awips2/psql/bin/psql -U awips -d metadata -c "ALTER TABLE gfe_dbid ADD COLUMN removeddate timestamp without time zone;"
if [ $? -ne 0 ]; then
   echo "FATAL: the update has failed!"
   exit 1
fi

echo "INFO: the update has completed successfully!"

exit 0
