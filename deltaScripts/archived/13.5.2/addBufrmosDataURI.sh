#!/bin/bash
# DR #2275 - this script is needd to add the dataURI column back into the
# bufrmosavn and bufrmoshpc tables.

PSQL="/awips2/psql/bin/psql"

${PSQL} -U awips -d metadata -c "ALTER TABLE bufrmosavn ADD COLUMN datauri character varying(255);"
if [ $? -ne 0 ]; then
      echo "ERROR: Failed to drop dataURI column for bufrmosavn"
      echo "FATAL: The update has failed."
      exit 1
   fi
${PSQL} -U awips -d metadata -c "ALTER TABLE bufrmoshpc ADD COLUMN datauri character varying(255);"
if [ $? -ne 0 ]; then
      echo "ERROR: Failed to add dataURI column for bufrmoshpc"
      echo "FATAL: The update has failed."
      exit 1
   fi
${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE bufrmosavn"
${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE bufrmoshpc"

echo "INFO: dataURI columns added successfully"
