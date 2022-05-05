#!/bin/bash
# DR #1992 - this update script will drop the refHour and validTime columns
# from the bufrua column, refTime has the exact same value.

PSQL="/awips2/psql/bin/psql"

${PSQL} -U awips -d metadata -c "ALTER TABLE bufrua DROP COLUMN IF EXISTS validtime, DROP COLUMN IF EXISTS refhour;"
