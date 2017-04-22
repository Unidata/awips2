#!/bin/sh
# DR 5810 - Drop replication related DB tables to clean up the obsolete ones and
#           force the ones that changed to be recreated with the latest versions
# Note: Incorporates changes for #5386 and #5638

PSQL=/awips2/psql/bin/psql
USER=awips

# Run on the dx1
echo "Clean up replication tables and drop the ones that have changed to force them to be recreated.";
   $PSQL -d metadata -U $USER << EOF
DROP TABLE IF EXISTS replication;
DROP TABLE IF EXISTS registryreplicationevents CASCADE;
DROP TABLE IF EXISTS registryreplicationsiteevents CASCADE;
DROP TABLE IF EXISTS registry_replication_sites CASCADE;
DROP TABLE IF EXISTS registry_replication_site_events CASCADE;
DROP TABLE IF EXISTS registry_replication_events CASCADE;
EOF
