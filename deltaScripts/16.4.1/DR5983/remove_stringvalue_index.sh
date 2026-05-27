#!/bin/sh
# DR 5983 - Remove ebxml.stringvalue_index

PSQL=/awips2/psql/bin/psql
USER=awipsadmin

echo "Dropping stringvalue_index from ebxml.value"
$PSQL -d metadata -U $USER -c "drop index if exists ebxml.stringvalue_index"

