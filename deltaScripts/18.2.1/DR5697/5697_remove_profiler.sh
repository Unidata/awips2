#!/bin/bash

# This script drops the profiler table Postgres databases.
# Run as root on all Postgres servers. Postgres must be running for this to
# work.
#
# Author: bsteffen

psql=/awips2/psql/bin/psql

echo INFO: Dropping profiler

${psql} --db metadata -U awipsadmin -c "DROP TABLE IF EXISTS profiler" 
${psql} --db metadata -U awipsadmin -c "DROP SEQUENCE IF EXISTS profilerseq"
${psql} --db metadata -U awipsadmin -c "delete from plugin_info where name = 'profiler'"
${psql} --db metadata -U awipsadmin -c "delete from purgejobs where plugin = 'profiler'"


echo INFO: Finished Dropping profiler
