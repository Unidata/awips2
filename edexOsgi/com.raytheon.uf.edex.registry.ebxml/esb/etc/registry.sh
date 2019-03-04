#!/bin/bash
##
##

##
# The registry is the ebXML registry software with additional Data Delivery
# components for downloading data from a provider and storing the resulting
# PluginDataObjects.
##

export MAX_MEM=1536 # in Meg

if [ $HIGH_MEM == "on" ]; then
    export MAX_MEM=2560
fi

export EDEX_DEBUG_PORT=5012
export HTTP_PORT=9588

export METADATA_POOL_MAX=20
export METADATA_POOL_TIMEOUT=60

export SOFT_REF_LRU_POLICY_MS_PER_MB=50
