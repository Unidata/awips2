#!/usr/bin/bash
# This script removes obsolete ISC Write Lock records from the cluster_task table
#
/awips2/psql/bin/psql  -U awips -d metadata -c "delete from cluster_task where name = 'ISC Write Lock' and details not like '%:%';"
