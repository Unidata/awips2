#!/bin/sh
# Updates the grid info_id index to include reftime
PSQL='/awips2/psql/bin/psql'

echo 'Updating grid_info_id_reftime_index'
$PSQL -d metadata -U awipsadmin << EOF
BEGIN;
drop index if exists grid_info_id_index;
drop index if exists grid_info_id_reftime_index;
create index grid_info_id_reftime_index on grid using btree (info_id, reftime);
COMMIT;
EOF

echo 'Index updated'
