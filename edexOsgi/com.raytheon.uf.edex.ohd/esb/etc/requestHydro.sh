#!/bin/bash
##
##

##
# Special request mode for ALR/SJU to support a second hydro localization
# and database at one site.
##

export MAX_MEM=1536 # in Meg

if [ $HIGH_MEM == "on" ]; then
    export MAX_MEM=2048
fi

export SERIALIZE_POOL_MAX_SIZE=24
export SERIALIZE_STREAM_INIT_SIZE_MB=2
export SERIALIZE_STREAM_MAX_SIZE_MB=8


export EDEX_DEBUG_PORT=5005
