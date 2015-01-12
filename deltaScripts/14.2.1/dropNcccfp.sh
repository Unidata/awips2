#!/bin/bash
# DR #2485 remove ncccfp from the database

PSQL="/awips2/psql/bin/psql"

SQL_COMMAND="
delete from plugin_info where name = 'ncccfp';
drop table if exists ncccfp;
drop sequence if exists ncccfpseq;
"
${PSQL} -U awips -d metadata -c "${SQL_COMMAND}"
