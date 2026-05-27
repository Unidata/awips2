#!/bin/bash

# This script should be run on the database server and drops several tables from the ebxml schema that are no longer used


echo INFO: Dropping unneccessary ebxml tables

/awips2/psql/bin/psql -U awipsadmin -d metadata -c " \
drop table if exists ebxml.action, ebxml.auditableevent, ebxml.auditableevent_action, ebxml.notification, ebxml.notification_auditableevent
"

echo INFO: Finished dropping unneccessary ebxml tables

