#!/bin/bash
# DR #4381 Add more space to the message column in notification table.

PSQL="/awips2/psql/bin/psql"

echo "INFO: Extending size of message column in event.notification table."

${PSQL} -U awips -d metadata -q -c "ALTER TABLE IF EXISTS events.notification ALTER COLUMN message TYPE
varchar(1024);"

echo "Done."