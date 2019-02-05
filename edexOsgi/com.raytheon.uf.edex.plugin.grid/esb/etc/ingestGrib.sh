#!/bin/bash
##
# The ingestGrib mode was designed to handle decoding and storage of all GRIB
# (both GRIB1 and GRIB2) data.  It was originally necessary due to instability
# in the decoder and 32-bit JVM memory limitations.  Therefore grib decoding
# could have dedicated memory and if the JVM went down, it would not affect
# the data flow of other datatypes.
##

numCores=`grep -c ^processor /proc/cpuinfo`                     # e.g. = 48
let "GRIB_DECODE_THREADS = numCores / 4"                          #      = 12
let "GRIB_SPLIT_THREADS = GRIB_DECODE_THREADS / 2"               #      = 6
if [ $GRIB_DECODE_THREADS -lt 4 ]; then
    GRIB_DECODE_THREADS=4
fi
if [ $GRIB_SPLIT_THREADS -lt 2 ]; then
    GRIB_SPLIT_THREADS=2
fi
let "MAX_MEM = GRIB_DECODE_THREADS * 400"                       # = 12 * 400 = 4800MB
let "GRIB_MAX_GRID_POINTS = GRIB_DECODE_THREADS * 25000000"     # = 12 * 25,000,000 = 300,000,000
let "GRID_PERSIST_THREADS = GRIB_DECODE_THREADS / 2"            # = 6
let "GRID_MAX_PERSIST_MEMORY_IN_MB = GRID_PERSIST_THREADS * 50" # = 6 * 50 = 300
let "METADATA_POOL_MAX = GRIB_DECODE_THREADS * 2"               # = 24

export INIT_MEM=128 # MB
export MAX_MEM
export GRIB_DECODE_THREADS
export GRIB_SPLIT_THREADS
export GRIB_MAX_GRID_POINTS
export GRID_PERSIST_THREADS
export GRID_POSTPROCESS_THREADS=1
export GRID_MAX_PERSIST_MEMORY_IN_MB
export METADATA_POOL_MAX
# Minimum coverage of a defined subgrid for the grid to be accepted
export SUB_GRID_COVERAGE_PERCENT=20
export GRID_MAX_GRIDS_PER_PERSIST=100
export GRID_MAX_MEMORY_IN_MB_PER_PERSIST=10

export EDEX_DEBUG_PORT=5007
