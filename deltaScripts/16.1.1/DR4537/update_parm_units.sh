#!/bin/bash
# DR #4537 - correct units in the parameter table.

PSQL="/awips2/psql/bin/psql"

echo "INFO: update parameter unit's column"

${PSQL} -U awips -d metadata -c "UPDATE awips.parameter SET unit='kg/m^2' WHERE abbreviation='VILIQ' ;"


echo "Done."