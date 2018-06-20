#!/bin/bash
##
# This software was developed and / or modified by Raytheon Company,
# pursuant to Contract DG133W-05-CQ-1067 with the US Government.
# 
# U.S. EXPORT CONTROLLED TECHNICAL DATA
# This software product contains export-restricted data whose
# export/transfer/disclosure is restricted by U.S. law. Dissemination
# to non-U.S. persons whether in the United States or abroad requires
# an export license or other authorization.
# 
# Contractor Name:        Raytheon Company
# Contractor Address:     6825 Pine Street, Suite 340
#                         Mail Stop B8
#                         Omaha, NE 68106
#                         402.291.0100
# 
# See the AWIPS II Master Rights File ("Master Rights File.pdf") for
# further licensing information.
##

##
# The ingestGrib mode was designed to handle decoding and storage of all GRIB
# (both GRIB1 and GRIB2) data.  It was originally necessary due to instability
# in the decoder and 32-bit JVM memory limitations.  Therefore grib decoding
# could have dedicated memory and if the JVM went down, it would not affect
# the data flow of other datatypes.
##

numCores=`grep -c ^processor /proc/cpuinfo`

let "GRIB_DECODE_THREADS=numCores / 2"

if [ $GRIB_DECODE_THREADS -gt 12 ]; then
    GRIB_DECODE_THREADS=12
elif [ $GRIB_DECODE_THREADS -lt 4 ]; then
    GRIB_DECODE_THREADS=4
fi

# sets bounds based on # of threads available
let "MAX_MEM = GRIB_DECODE_THREADS * 196" # in Meg
let "GRIB_MAX_GRID_POINTS = GRIB_DECODE_THREADS * 25000000"
let "GRID_PERSIST_THREADS = GRIB_DECODE_THREADS / 2"
let "GRID_POSTPROCESS_THREADS = 1"
let "GRID_MAX_PERSIST_MEMORY_IN_MB = GRID_PERSIST_THREADS * 50"
let "METADATA_POOL_MAX = GRIB_DECODE_THREADS * 2"

export INIT_MEM=128 # in Meg
export MAX_MEM
export GRIB_DECODE_THREADS
export GRIB_MAX_GRID_POINTS
export GRID_PERSIST_THREADS
export GRID_POSTPROCESS_THREADS
export GRID_MAX_PERSIST_MEMORY_IN_MB
export METADATA_POOL_MAX

export EDEX_DEBUG_PORT=5007

