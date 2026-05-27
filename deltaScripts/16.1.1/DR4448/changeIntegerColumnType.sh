#!/bin/bash
# DR #4448 - Separate ebxml registry code from Data Delivery

PSQL="/awips2/psql/bin/psql"

echo "INFO: Changing column type of integervalue in ebxml.value to bigint"

${PSQL} -U awips -d metadata -q -c "ALTER TABLE ebxml.value ALTER COLUMN integervalue TYPE bigint;"

echo "Done." 
