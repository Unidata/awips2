#!/bin/bash
# DR #5638 - Drops the EBXML table on a registry to force synchronization to trigger.

##############################################
### DO NOT RUN THIS ON A CENTRAL REGISTRY! ###
##############################################

# Drop and re-create the ebxml schema, this forces a synchronization
echo "Dropping and recreate ebxml schema to force a sync..."
psql -d metadata -U awips << EOF
DROP SCHEMA ebxml CASCADE;
CREATE SCHEMA ebxml AUTHORIZATION awips;
EOF
