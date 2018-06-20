#!/bin/bash

PSQL="/awips2/psql/bin/psql"

# vaa plugin was in rare occassions inserting records without a time which are then never purged
${PSQL} -U awips -d metadata -c "delete from vaa_location where parentid in (select recordid from vaa_subpart where parentid in (select id from vaa where reftime is NULL))"
${PSQL} -U awips -d metadata -c "delete from vaa_subpart where parentid in (select id from vaa where reftime is NULL)"
${PSQL} -U awips -d metadata -c "delete from vaa where reftime is NULL"

tables=$(psql -U awips -d metadata -tc "select table_name from information_schema.columns where column_name = 'reftime'")

echo "Updating record tables to disallow null times"
for table in $tables
do
    echo "Updating $table"
    psql -U awips -d metadata -c "ALTER TABLE $table ALTER COLUMN reftime SET NOT NULL"
done
echo "Done"
