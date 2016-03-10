#! /bin/bash
# DR #5119 - This script drops the bufrquikscat table

PSQL="/awips2/psql/bin/psql"

echo "INFO: Removing bufrquikscat table"

${PSQL} -U awips -d metadata -c "DROP TABLE IF EXISTS bufrquikscat;"
${PSQL} -U awips -d metadata -c "DROP SEQUENCE IF EXISTS bufrquikscatseq;"

