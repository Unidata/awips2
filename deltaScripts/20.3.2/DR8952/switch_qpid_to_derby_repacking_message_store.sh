#!/bin/bash

# Delta script for Omaha #8952
# This script changes Qpid's message store type from DERBY to DERBY-REPACK.
#
# Run on the qpid server
#
# Author: njensen

echo "$0: starting"

echo "$0: updating /awips2/qpid/edex/config/edex.json"
sed -i 's/"type" : "DERBY",/"type" : "DERBY-REPACK",/g' /awips2/qpid/edex/config/edex.json

if [ -f "/awips2/qpid/edex-alr/config/edex-alr.json" ]; then
    echo "$0: updating /awips2/qpid/edex-alr/config/edex-alr.json"
    sed -i 's/"type" : "DERBY",/"type" : "DERBY-REPACK",/g' /awips2/qpid/edex-alr/config/edex-alr.json
fi

echo "$0: finished"

