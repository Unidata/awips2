#!/bin/bash
# DR #1538 Expand the name column in the slot table

PSQL="/awips2/psql/bin/psql"

${PSQL} -U awips -d metadata -c "ALTER TABLE ebxml.slot ALTER COLUMN name TYPE character varying(1024)"