#!/bin/bash
# DR #5810 - Update configuration and database tables for replication and synchronization
# Note: Incorporates changes for #5386 and #5638

# Run on the same box the registry is installed on
echo "Updating com.raytheon.uf.edex.registry.ebxml.properties configuration..."
# Remove any instances of ebxml-federation-repl-threads from the files first, so we don't end up with duplicates.
find /awips2/edex/conf/resources/site  -name "com.raytheon.uf.edex.registry.ebxml.properties" -exec sed -i 's/ebxml-federation-repl-threads=[0-9]//g' {} +
# Find all instances of ebxml-federation-sync-threads. Set it to 1 and add 'ebxml-federation-repl-threads=3' on the line below.
find /awips2/edex/conf/resources/site  -name "com.raytheon.uf.edex.registry.ebxml.properties" -exec sed -i 's/ebxml-federation-sync-threads=[0-9]/ebxml-federation-sync-threads=1\nebxml-federation-repl-threads=6/g' {} +
