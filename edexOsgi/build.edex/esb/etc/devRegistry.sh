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

export MAX_MEM=1536 # in Meg
export MAX_PERM_SIZE=192m
export METADATA_POOL_MIN=10
export EDEX_DEBUG_PORT=5012
export EDEX_JMX_PORT=1620
export LOG_CONF=logback-registry.xml
export MGMT_PORT=9605
export EBXML_REGISTRY_FEDERATION_ENABLED=false
