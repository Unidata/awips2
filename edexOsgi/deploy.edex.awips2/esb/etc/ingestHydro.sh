#!/bin/bash
export MAX_MEM=796 # in Meg
if [ $HIGH_MEM == "on" ]; then
    export MAX_MEM=1024
fi
export METADATA_POOL_MAX=25
export EDEX_DEBUG_PORT=5006
export EDEX_JMX_PORT=1617
export MGMT_PORT=9602
