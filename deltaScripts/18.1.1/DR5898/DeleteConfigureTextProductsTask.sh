#!/usr/bin/bash
# This script removes obsolete GfeConfigureTextProducts records from the cluster_task table
#
/awips2/psql/bin/psql  -U awips -d metadata -c "delete from cluster_task where name = 'GfeConfigureTextProducts';"
