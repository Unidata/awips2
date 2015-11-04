#! /bin/bash
# DR #5019 - This script resizes the gfe_parmid.parmlevel column.

PSQL="/awips2/psql/bin/psql"

echo "INFO: Updating gfe_parmid"

views=( "gfe_view" "gfe_locks_view" )
for view in ${views[@]} ; do 
	${PSQL} -U awips -d metadata -c "DROP VIEW IF EXISTS ${view} ;"
done

${PSQL} -U awips -d metadata -c "ALTER TABLE IF EXISTS gfe_parmid ALTER COLUMN parmlevel TYPE character varying(16) ;"

${PSQL} -U awips -d metadata -f gfeViews.sql

