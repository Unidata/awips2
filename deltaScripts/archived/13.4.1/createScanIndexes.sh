#!/bin/bash
# DR #1926 - this update script will create a scan index

PSQL="/awips2/psql/bin/psql"

echo "INFO: Creating scan_icao_type_idx"
${PSQL} -U awips -d metadata -c "CREATE INDEX scan_icao_type_idx ON scan USING btree (icao COLLATE pg_catalog.\"default\", type COLLATE pg_catalog.\"default\");"

if [ $? -ne 0 ]; then
   echo "ERROR: Failed to create index."
   echo "FATAL: The update has failed."
   exit 1
fi

echo "INFO: Index created successfully!"

exit 0
