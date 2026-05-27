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
# The centralRegistry mode was designed to be an ebXML registry plus all the
# components necessary to harvest data from a data provider and manage
# SBN/Shared Subscriptions. Anything registry-related that is meant to run
# only once for all of the NWS would run in this mode.
##

export MAX_MEM=2048 # in Meg

if [ $HIGH_MEM == "on" ]; then
    # dds1 has 64G of memory, replicate to all sites
    export MAX_MEM=24576
fi

export HTTP_PORT=9588
export EDEX_DEBUG_PORT=5011

export METADATA_POOL_TIMEOUT=60
export CLUSTER_ID=NCF

export SOFT_REF_LRU_POLICY_MS_PER_MB=50

export DATASTORE_PROVIDER=pypies
export METADATA_POOL_MAX=250
