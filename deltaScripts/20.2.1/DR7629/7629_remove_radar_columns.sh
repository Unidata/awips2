#/bin/bash
#
# This un-delta script removes 2 columns from the radar table added by the original 7269 delta script
# This script should be run on dx1 after Postgres is up and running
#
/awips2/psql/bin/psql -U awipsadmin -d metadata -c "ALTER TABLE radar DROP COLUMN IF EXISTS deltatime; \
                                                    ALTER TABLE radar DROP COLUMN IF EXISTS scanType;"
echo "Un-delta script for DR #7629 complete"
