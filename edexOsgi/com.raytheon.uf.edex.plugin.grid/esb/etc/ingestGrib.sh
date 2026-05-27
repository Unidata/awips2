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
# Set the expected estimated size of the largest grid to handle worst case
MAX_GRID_SIZE_IN_MB=400

# On each persist thread, Ignite reserves a byte array that is the size of the
# largest object that has been serialized on that thread, rounded up to the
# nearest power of 2. Calculate that value.
MAX_GRID_SIZE_IN_MB_POWER_2=1
while [[ $MAX_GRID_SIZE_IN_MB_POWER_2 -lt $MAX_GRID_SIZE_IN_MB ]]; do
    let "MAX_GRID_SIZE_IN_MB_POWER_2 = MAX_GRID_SIZE_IN_MB_POWER_2 * 2"
done

# Determine number of decode threads. Note that decoding and persisting are
# done on the same thread. Only set it if it isn't already to allow local
# profile entries to override this.
if [[ "$GRIB_DECODE_THREADS" = "" ]]; then
    if [[ "$numCores" -ge 12 ]]; then
        GRIB_DECODE_THREADS=16
    else
        GRIB_DECODE_THREADS="${numCores}"
    fi
fi

# Set to keep enough room for the worst case, which is that there are max size
# grids in every decode/persist thread and Ignite has reserved the max grid size
# rounded up the nearest power of 2 in every decode/persist thread as well.
# Default value of 14592MB
let "MAX_MEM = MAX_GRID_SIZE_IN_MB * GRIB_DECODE_THREADS + MAX_GRID_SIZE_IN_MB_POWER_2 * GRIB_DECODE_THREADS"

export SERIALIZE_STREAM_INIT_SIZE_MB=50
export SERIALIZE_STREAM_MAX_SIZE_MB=50
export SERIALIZE_POOL_MAX_SIZE=$GRIB_DECODE_THREADS

export MAX_MEM

# sets bounds based on # of threads available
let "GRIB_MAX_GRID_POINTS = GRIB_DECODE_THREADS * 25000000"
let "METADATA_POOL_MAX = GRIB_DECODE_THREADS * 2"
export GRIB_DECODE_THREADS
export GRIB_MAX_GRID_POINTS
export METADATA_POOL_MAX

export INIT_MEM=128 # in Meg
export GRID_POSTPROCESS_THREADS=1
export EDEX_DEBUG_PORT=5007
export EDEX_JMX_PORT=1618
export MGMT_PORT=9603

export IGNITE_CLUSTER_1_COMM_PORT=47103
export IGNITE_CLUSTER_1_DISCO_PORT=47503
export IGNITE_CLUSTER_2_COMM_PORT=47108
export IGNITE_CLUSTER_2_DISCO_PORT=47508
