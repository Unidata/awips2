#!/bin/bash
# DR #2493 remove mesowest from the database

PSQL="/awips2/psql/bin/psql"

SQL_COMMAND="
delete from plugin_info where name = 'mesowest';
drop table if exists mesowest;
drop sequence if exists mesowestseq;
"
${PSQL} -U awips -d metadata -c "${SQL_COMMAND}"
