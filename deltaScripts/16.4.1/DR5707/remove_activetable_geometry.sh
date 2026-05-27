#!/bin/bash
# DR #5707 - This script alters the table "activetable" and drops the geometry column.
# DR #5679's delta script is expected to have been run, adding the awipsadmin user.
# If it has not been run, run it first or change USER to "awips"

PSQL="/awips2/psql/bin/psql"
USER="awipsadmin"

echo "Dropping activetable's geometry column."
${PSQL} -U ${USER} -d metadata -c "ALTER TABLE activetable DROP COLUMN IF EXISTS geometry;"
echo "Dropping practice_activetable's geometry column."
${PSQL} -U ${USER} -d metadata -c "ALTER TABLE practice_activetable DROP COLUMN IF EXISTS geometry;"

echo "Done."
