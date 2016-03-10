#!/bin/bash
# DR #5283 - Remove support for NGM. Drops the bufrmosngm table and removes 
# references to the plugin from purgejobs and plugin_info. 

PSQL="/awips2/psql/bin/psql"


echo "INFO: Attempting to drop bufrmosngm table"

${PSQL} -U awips -d metadata -c "DROP TABLE IF EXISTS bufrmosngm"

echo "INFO: Attempting to remove bufrmosNGM plugin references from other tables"

${PSQL} -U awips -d metadata -c "DELETE FROM plugin_info where name = 'bufrmosNGM'"
${PSQL} -U awips -d metadata -c "DELETE FROM purgejobs where plugin = 'bufrmosNGM'"

echo "INFO: Done."