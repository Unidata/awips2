#!/bin/bash
# DR #4347 - rename the startTime column to issueTime in the productData table.

PSQL="/awips2/psql/bin/psql"

ADDTABLE="
DO \$\$
BEGIN
    ALTER TABLE IF EXISTS productData ADD COLUMN issuetime timestamp;
EXCEPTION
    WHEN duplicate_column THEN RAISE INFO 'column issuetime already exists in productData.';
END;
\$\$
"

echo "INFO: removing productData starttime column"

${PSQL} -U awips -d metadata -q -c "ALTER TABLE IF EXISTS productData DROP COLUMN IF EXISTS starttime;"

echo "INFO: adding productData issuetime column"

${PSQL} -U awips -d metadata -q -c "${ADDTABLE}"

echo "Done."