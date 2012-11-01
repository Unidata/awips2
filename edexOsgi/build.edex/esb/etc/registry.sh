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

export INIT_MEM=512 # in Meg

if [ $HIGH_MEM_FLAG == "on" ]; then
    export MAX_MEM=1536 # in Meg
else
    export MAX_MEM=896 # in Meg
fi
export METADATA_POOL_MIN=10
export EDEX_DEBUG_PORT=5011
export EDEX_JMX_PORT=1622
export LOG4J_CONF=log4j-registry.xml
export MGMT_PORT=9607

