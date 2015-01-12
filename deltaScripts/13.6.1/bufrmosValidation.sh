#!/bin/bash
# DR #1051 remove invalid bufrmos locations

PSQL="/awips2/psql/bin/psql"

${PSQL} -U awips -d metadata -c "DELETE FROM bufrmosmrf WHERE location_id IN (SELECT DISTINCT id FROM bufrmos_location WHERE latitude > 90 or latitude < -90);"
${PSQL} -U awips -d metadata -c "DELETE FROM bufrmoshpc WHERE location_id IN (SELECT DISTINCT id FROM bufrmos_location WHERE latitude > 90 or latitude < -90);"
${PSQL} -U awips -d metadata -c "DELETE FROM bufrmos_location WHERE latitude > 90 or latitude < -90;"
${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE bufrmosmrf;"
${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE bufrmoshpc;"
${PSQL} -U awips -d metadata -c "VACUUM FULL ANALYZE bufrmos_location;"