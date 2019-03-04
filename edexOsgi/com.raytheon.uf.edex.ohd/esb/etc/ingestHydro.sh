#!/bin/bash
##
##

##
# Special ingest mode for ALR/SJU to support a second hydro localization and
# database at one site.
##

export MAX_MEM=796 # in Meg

if [ $HIGH_MEM == "on" ]; then
    export MAX_MEM=1024
fi

export METADATA_POOL_MAX=25
export EDEX_DEBUG_PORT=5006
