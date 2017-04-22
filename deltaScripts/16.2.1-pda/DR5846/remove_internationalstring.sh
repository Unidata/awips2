#!/bin/sh
# DR 5846 - Remove support for international/localized string

PSQL=/awips2/psql/bin/psql
USER=awips

updateTableField(){
table="$1"
col="$2"

row=$( $PSQL -d metadata -U $USER -tc "select column_name from information_schema.columns where table_schema='ebxml' and table_name='$table' and column_name='$col'" )
table="ebxml.$table"

if [ "$row" == "" ]; then
   echo "Updating $table - Adding column $col"

   $PSQL -d metadata -U $USER << EOF
BEGIN;
alter table $table add column "$col" character varying($3);
update $table set $col = lstring.value from (select string_id, value from ebxml.localizedstring) lstring where $table.${col}_id = lstring.string_id;
alter table $table drop column ${col}_id;
COMMIT;
EOF

   if [ $? -ne 0 ]; then
      echo "ERROR: Failed to add $col column for table $1"
      echo "FATAL: The update has failed."
      exit 1
   fi
else
   echo "Skipping $table $col - column already added"
fi
}

updateTable() {
updateTableField $1 name 255
updateTableField $1 description 1024
}

echo "DR 5846 - Adding name and description columns to ebxml tables"

tables=( 'association' 'auditableevent' 'classification' 'classificationnode' 'classificationscheme' 'comment' 'externalidentifier' 'externallink' 'extrinsicobject' 'federation' 'notification' 'organization' 'parameter' 'person' 'querydefinition' 'registry' 'registryobject' 'registrypackage' 'role' 'service' 'servicebinding' 'serviceendpoint' 'serviceinterface' 'subscription' 'workflowaction' )

for table in "${tables[@]}"; do
   updateTable $table
done

$PSQL -d metadata -U $USER << EOF
BEGIN;
alter table ebxml.value drop column if exists internationalstring_id;
drop table if exists ebxml.localizedstring;
drop table if exists ebxml.internationalstring;
COMMIT;
drop index concurrently if exists ebxml.value_idx;
drop index concurrently if exists ebxml.slot_value_id_idx;
create index concurrently slot_value_id_idx on ebxml.value using btree (value_id);
EOF

