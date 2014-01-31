#!/bin/bash
# DR #2537 - this update script will drop the fcstseconds and timeobs columns
# from the modelsounding table, refTime and forecasttime have the exact same values.

PSQL="/awips2/psql/bin/psql"

${PSQL} -U awips -d metadata -c "ALTER TABLE modelsounding DROP COLUMN IF EXISTS fcstseconds, DROP COLUMN IF EXISTS timeobs;"
