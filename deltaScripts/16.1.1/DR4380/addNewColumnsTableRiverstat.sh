#!/bin/bash
# DR #4380 - add issuestg and issueflow columns to the riverstat table.

PSQL="/awips2/psql/bin/psql"
# Use local Hydro DB (hd_ob92<site>)
DB="hd_ob92oax"

ADDCOLUMNS="
DO \$\$
BEGIN
    ALTER TABLE IF EXISTS riverstat ADD COLUMN issuestg double precision,
       ADD COLUMN issueflow double precision;
EXCEPTION
    WHEN duplicate_column THEN RAISE INFO 'columns already exist in riverstat.';
END;
\$\$
"

echo "INFO: adding issuestg and issueflow columns to riverstat table"

${PSQL} -U awips -d ${DB} -q -c "${ADDCOLUMNS}"

echo "Done."

