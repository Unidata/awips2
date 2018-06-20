#!/bin/bash

# 6402 - Add an index on source to the binlightning table in the metadata database.
#
# Author: mapeters
# Oct 31, 2017

psql=/awips2/psql/bin/psql
index=binlightning_sourceIndex
table=awips.binlightning

sql="CREATE INDEX IF NOT EXISTS ${index} ON ${table} USING btree(source);"
${psql} -U awipsadmin -d metadata -c "${sql}"
if [[ $? -ne 0 ]]; then
    echo "ERROR: Failed to create index ${index} on table ${table}."
    echo "FATAL: The update has failed."
    exit 1
fi

echo "INFO: the update has completed successfully!"
exit 0
