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

export IGNITE_CLUSTER_1_COMM_PORT=47104
export IGNITE_CLUSTER_1_DISCO_PORT=47504
export IGNITE_CLUSTER_2_COMM_PORT=47109
export IGNITE_CLUSTER_2_DISCO_PORT=47509
