#!/bin/bash
# DR #2505 Disable deployment of recco plugin

PSQL="/awips2/psql/bin/psql"

SQL_COMMAND="
delete from plugin_info where name = 'recco';
drop table if exists recco;
drop sequence if exists reccoseq;
"
${PSQL} -U awips -d metadata -c "${SQL_COMMAND}"
