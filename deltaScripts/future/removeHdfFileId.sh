#!/bin/sh
# This script should be run on dx1 as part of next delivery to remove the hdfFileId columns

tablesToUpdate=`psql -U awips -d metadata -t -c "select table_name from information_schema.columns where table_catalog='metadata' and table_schema='awips' and column_name = 'hdffileid';"`

for table in $tablesToUpdate
do
   psql -U awips -d metadata -t -c "ALTER TABLE $table DROP COLUMN hdffileid"
done

