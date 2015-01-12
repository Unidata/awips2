#!/bin/bash

tables=$(psql -U awips -d metadata -tc "select table_name from information_schema.columns where column_name = 'reftime'")

echo "Updating record tables to disallow null times"
for table in $tables
do
    echo "Updating $table"
    psql -U awips -d metadata -c "ALTER TABLE $table ALTER COLUMN reftime SET NOT NULL"
done
echo "Done"
