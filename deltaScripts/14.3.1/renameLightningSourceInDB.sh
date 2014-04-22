#!/bin/bash
# DR #2667 Add binlightning support to Data Access Framework

PSQL="/awips2/psql/bin/psql"

SQL_COMMAND="
ALTER TABLE binlightning RENAME COLUMN lightsource TO source;
"
${PSQL} -U awips -d metadata -c "${SQL_COMMAND}"
