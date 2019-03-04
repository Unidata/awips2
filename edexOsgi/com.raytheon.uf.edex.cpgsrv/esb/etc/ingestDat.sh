#!/bin/bash
##
##

##
# The ingestDat mode was designed to handle server side processing of the
# DAT (Decision Assist Tool) suite.  This primarily creates FFMP, SCAN, and
# related data upon notification of the successful ingest of specific input
# datatypes.  It was originally necessary due to instability and 32-bit JVM
# memory limitations.  Therefore ingestDat could have dedicated memory and if
# the JVM went down, it would not affect other services.
##

export MAX_MEM=1856 # in Meg

if [ $HIGH_MEM == "on" ]; then
    export MAX_MEM=2560
fi

export METADATA_POOL_MAX=25
export EDEX_DEBUG_PORT=5008

