#!/bin/bash
# This script will delete the Ingest.Grib queue from qpid.
# qpid must be running when this script is executed.
#
# This script will also remove the large grib file lock
#
# This update is required with 14.2.1.
#

PSQL="/awips2/psql/bin/psql"

echo "INFO: Deleting Ingest.Grib queue."

curl -X DELETE http://cp1f:8180/rest/queue/edex/Ingest.Grib > /dev/null

echo "INFO: Deleting GribIngestLargeFile cluster locks."

${PSQL} -U awips -d metadata -c "delete from cluster_task where name = 'GribIngestLargeFile';"

echo "INFO: The update was applied successfully."

exit 0
