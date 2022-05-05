#!/bin/bash

echo "Updating radar table to include volume scan number."

SQL="
DO \$\$
BEGIN
    ALTER TABLE radar ADD COLUMN volumescannumber integer;
EXCEPTION
    WHEN duplicate_column THEN RAISE NOTICE 'column volumescannumber already exists in radar.';
END;
\$\$
"

/awips2/psql/bin/psql -U awips -d metadata -c "${SQL}"
if [[ $? != 0 ]]
then
    echo "Failed to update radar table."
    exit 1
fi
/awips2/psql/bin/psql -U awips -d metadata -c "UPDATE radar SET volumescannumber=0 WHERE volumescannumber IS NULL;"

echo "Done"
