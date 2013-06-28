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
if [ "$EDEX_ARCH" == "64-bit" ]; then
    export MAX_MEM=2048 # in Meg
else
    export MAX_MEM=1280 # in Meg
fi
export SERIALIZE_POOL_MAX_SIZE=24
export SERIALIZE_STREAM_INIT_SIZE_MB=2
export SERIALIZE_STREAM_MAX_SIZE_MB=8


export JMS_POOL_MIN=16
export JMS_POOL_MAX=32
export EDEX_DEBUG_PORT=5005
export EDEX_JMX_PORT=1616
export MGMT_PORT=9601
