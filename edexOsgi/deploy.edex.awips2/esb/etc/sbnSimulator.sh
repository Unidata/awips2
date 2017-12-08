#!/bin/bash
export MAX_MEM=1536 # in Meg
if [ $HIGH_MEM == "on" ]; then
    export MAX_MEM=2560
fi
export MAX_PERM_SIZE=192m
export EDEX_DEBUG_PORT=5013
export MGMT_PORT=9606
export METADATA_POOL_MAX=20
export METADATA_POOL_TIMEOUT=60
export SOFT_REF_LRU_POLICY_MS_PER_MB=50
