#!/bin/bash
# DR #5269 - This script drops table fxatext.watchwarn

PSQL="/awips2/psql/bin/psql"

echo "INFO: Dropping table fxatext.watchwarn."

${PSQL} -U awips -d fxatext -q -c "DROP TABLE IF EXISTS watchwarn CASCADE;"

if [ $? -eq 0 ]; then
      echo "INFO: watchwarn table successfully dropped."
else
      echo "WARNING: Unable to drop watchwarn table."
      exit 1
fi

