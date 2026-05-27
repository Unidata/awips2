#!/bin/sh
# DR 5846 - Remove SlotValueType association

PSQL=/awips2/psql/bin/psql
USER=awips

echo "Dropping slot_id from ebxml.value"
$PSQL -d metadata -U $USER -c "alter table ebxml.value drop column if exists slot_id;"

