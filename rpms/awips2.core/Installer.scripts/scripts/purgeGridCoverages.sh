#!/usr/bin/bash

# Omaha #6315
# This script should be occasionally run on the database server to remove unnecessary gridcoverages.
# This purging is not done within edex because grid coverages don't leak fast enough to cause problems.
# This script should be run while all edices are stopped to avoid inconsistent caches in edex memory.
# This script should only be needed a few times a year, although it is generally fast and harmless.


psql -U awips -d metadata -c "delete from gridcoverage where id not in (select distinct location_id from grid_info);"
