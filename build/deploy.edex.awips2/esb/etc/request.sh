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
export INIT_MEM=128 # in Meg
export MAX_MEM=2024 # in Meg
export SERIALIZE_POOL_MAX_SIZE=500
export SERIALIZE_STREAM_INIT_SIZE_MB=2
export SERIALIZE_STREAM_MAX_SIZE_MB=8

export EDEX_DEBUG_PORT=5005
export EDEX_JMX_PORT=1616
export MGMT_PORT=9601
export HTTP_PORT=9581
# from data delivery centralRegistry.sh
export METADATA_POOL_TIMEOUT=60
export CLUSTER_ID=NCF
export SOFT_REF_LRU_POLICY_MS_PER_MB=50
