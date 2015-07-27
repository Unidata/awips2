#!/bin/bash
# DR #4479 - add editableEntries column to the productData table.

PSQL="/awips2/psql/bin/psql"

ADDCOLUMN="
DO \$\$
BEGIN
    ALTER TABLE IF EXISTS productData ADD COLUMN editableEntries bytea;
EXCEPTION
    WHEN duplicate_column THEN RAISE INFO 'column editableEntries already exists in productData.';
END;
\$\$
"

echo "INFO: adding editableEntries column to productData table"

${PSQL} -U awips -d metadata -q -c "${ADDCOLUMN}"

echo "Done."
